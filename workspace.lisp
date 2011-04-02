;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is a collection of 'workspace' code for exploring a
;; 4store RDF knowledgebase using Steel Bank Common Lisp. The code has
;; been used with Steel Bank Common Lisp, but should run without
;; issues in many if not most Common Lisp implementations.
;; 
;; Copyright (c) 2011, Patrick D. Logan
;; All rights reserved.
;;
;; See COPYING for more information. The license is the "new and
;; simplified BSD license" See
;; http://www.opensource.org/licenses/bsd-license
;;
;; For installing 4store on ubuntu see:
;; http://patricklogan.blogspot.com/2011/03/building-4store-on-ubuntu-1010.html
;; 
;; Create a new 4store knowledgebase:
;; 4s-backend-setup sample
;;
;; Start the knowledgebase server as a daemon:
;; 4s-backend sample
;;
;; Start a sparql endpoint http server for the knowledgebase:
;; 4s-httpd  -p 8080 -D sample
;;
;; -D prevents the http server from daemonizing in order to see
;; -messages interactively.
;;
;; Status of the (empty) KB is now at http://localhost:8080/status/
;;
;; Load the sample data (an RDF/XML file) using curl (here) or a POST
;; from lisp (below):
;; curl -v -T organogram-co-2010-10-31-index.rdf 'http://localhost:8080/data/organogram-co-2010-10-31-index'
;;
;; The data file comes from
;; http://source.data.gov.uk/data/reference/organogram-co/2010-10-31/index.rdf
;; and other formats are also available there.
;;
;; Install Steel Bank Common Lisp from http://www.sbcl.org/
;;
;; Install quicklisp for finding and managing CL libraries (similar to
;; ruby's gems)
;; http://www.quicklisp.org/

;; Quicklisp will be able to load this local library. Make sure the
;; directory path has a / on the end or it is not recognized as a
;; directory. See
;; http://common-lisp.net/project/asdf/asdf/Configuring-ASDF.html
;; Download cl-rdfxml from http://www.cs.rpi.edu/~tayloj/CL-RDFXML/
(push #p"/home/patrick/software/cl-rdfxml/cl-rdfxml_0.9/" asdf:*central-registry*)

(ql:quickload "puri")          ; Working with URIs.
(ql:quickload "drakma")        ; An HTTP client.
(ql:quickload "cxml")          ; XML parsing.
(ql:quickload "fare-matcher")  ; Lisp pattern matching.
;(ql:quickload "cl-utilities")  ; Em, utilities not used currently in this file.
(ql:quickload "cl-rdfxml")     ; RDF/XML parsing into RDF-specific objects.
;(ql:quickload "hunchentoot")   ; A web server, but only import a couple URL utilities.

(use-package 'drakma)
(use-package 'fare-matcher)
;(use-package 'cl-utilities)
(use-package 'cl-rdfxml)
(import 'puri::render-uri)
(import 'puri::uri-p)
;(import 'hunchentoot::url-encode)
;(import 'hunchentoot::url-decode)

(defun put-data ()
  "This PUTs the data file at the given URL. Equivalalent of: curl -v -T organogram-co-2010-10-31-index.rdf 'http://localhost:8080/data/organogram-co-2010-10-31-index'"
  (http-request "http://localhost:8080/data/organogram-co-2010-10-31-index"
                :method :put
                :content (open #p"/home/patrick/dev/sbcl-4store/organogram-co-2010-10-31-index.rdf" :element-type '(unsigned-byte 8))
                :content-type "application/rdf+xml" :content-length t))

(defun sparql-server-status ()
  "Return the status of the 4store sparql server from lisp. http-request returns multiple values and the second one is the numerical http status."
  (nth-value 1 (http-request "http://localhost:8080/status")))

;; Tell drakma to treat these additional types as text rather than
;; binary.
(setq *text-content-types* (list* '("application". "sparql-results+xml")
				  '("application" . "rdf+xml")
				  *text-content-types*))

(defun remove-if-typep (type list)
  "A shorthand way to remove an unwanted type of data from a list."
  (remove-if (lambda (x) (typep x type)) list))

(defun select-rdfs-classes ()
  "Select all the RDFS classses in the knowledgebase. Return the multiple values from the query POSTed to the knowledgebase's sparql http end-point. The first value is the body of the response in the sparql query results XML format."
  (http-request "http://localhost:8080/sparql/" 
		:method :post  
		:parameters `(("query" . "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                	                  prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                	                  select distinct ?type 
                	                  where { 
                	                      ?x a ?type .
                	                  } 
                	                  order by ?type"))))

(defun construct-persons ()
  "Select the FOAF (http://www.foaf-project.org/) Person instances with FOAF names, and all other triples having the Person as the subject. Return the multiple values from the query POSTed to the knowledgebase's sparql http end-point. The first value is a graph constructed from the selected triples in RDF/XML format."
  (http-request "http://localhost:8080/sparql/" 
		:method :post  
		:parameters `(("query" . "prefix foaf: <http://xmlns.com/foaf/0.1/>
                                          construct {
					    ?person 
					      a foaf:Person ;
					      foaf:name ?name ;
					      ?prop ?value .
					  } where { 
					    ?person a foaf:Person ;
					      foaf:name ?name ;
					      ?prop ?value .                        
                                          }"))))

(defun map-uris-to-strings (list)
  "Return a copy of the given list with top-level URIs rendered as strings."
  (mapcar (lambda (x)
	    (if (uri-p x)
		(with-output-to-string (stream)
		  (render-uri x stream))
	      x)) list))

(defun extract-persons ()
  "Use cl-rdfxml to parse the RDF/XML of FOAF Persons and related triples. Return the triples as a list of three-lists with URIs rendered as strings."
  (let ((persons '()))
    (parse-document (lambda (s p o)
		      (push (map-uris-to-strings (list s p o))
			    persons))
		    (construct-persons))
    ;; not really necessary to reverse, but it's nice to be in the
    ;; same order as the RDF/XML.
    (reverse persons)))

(defun parse-result (xml-result)
  "Use cxml to parse a sparql result XML formatted string into a list structure."
  (cxml:parse xml-result (cxml-xmls:make-xmls-builder)))

;; More meaningful names.
(setf (symbol-function 'extract-data) #'sixth)
(setf (symbol-function 'extract-rows) #'cdddr)

(defun extract-rdfs-classes ()
  "Return a list of the RDFS class URIs in the knowledgebase. Use fare-matcher to pattern-match over a list structure parsed from the sparql XML format. Underscores in the pattern are 'don't care' positions. The 'uri' variable is the position of interest."
  (mapcar (lambda (item) 
	    (letm (list _ _ _ (list _ _ (list _ _ uri)) _) 
		  item 
		  uri)) 
	  (remove-if-typep 'string (extract-rows
				    (extract-data
				     (parse-result
				      (select-rdfs-classes)))))))
 