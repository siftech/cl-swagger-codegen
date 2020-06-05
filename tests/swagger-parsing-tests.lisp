(defpackage :swagger-parsing-tests
  (:import-from #:cl-swagger
                #:parse-path-parameters
                #:split-path
                )
  (:use #:cl #:fiveam))

(in-package :swagger-parsing-tests)

(defparameter +path-with+
  '(:|/microservices/watch/{watch_id}|
 (:|get| (:|tags| "Registry")
  (:|summary| . "Returns the details of the specified watch.")
  (:|description|
   . "Returns the details of the specified watche for microservices being monitored by the registry.")
  (:|operationId| . "getMicroserviceWatch")
  (:|parameters|
   ((:|name| . "watch_id") (:|in| . "path") (:|required| . T)
    (:|description| . "Retrieve the open microservice watch")
    (:|schema| (:|type| . "string"))))
  (:|responses|
   (:|200|
    (:|description|
     . "Success - returns the watch definition in the registry")
    (:|content|
     (:|application/json|
      (:|schema|
       (:|$ref| . "#/components/schemas/MicroserviceWatch")))))
   (:|204|
    (:|description|
     . "Success - but there is no content in the registry"))
   (:|400| (:|description| . "Bad request / input format exception"))
   (:|500| (:|description| . "Registry error"))))
 (:|delete| (:|tags| "Registry")
  (:|summary| . "Delete the microservice watch")
  (:|description|
   . "Delete the specified microservice watch (no more POST calls will be made to the formerly specified endpoint)")
  (:|operationId| . "deleteMicroserviceWatch")
  (:|parameters|
   ((:|name| . "watch_id") (:|in| . "path") (:|required| . T)
    (:|description| . "Delete the open microservice watch")
    (:|schema| (:|type| . "string"))))
  (:|responses| (:|200| (:|description| . "Successful deletion"))
   (:|400| (:|description| . "Bad request / input format exception"))
   (:|404| (:|description| . "Service_id not found"))
   (:|500| (:|description| . "Registry error"))))))

(defparameter +path-without+
  '(:|/microservices/watch|
 (:|get| (:|tags| "Registry")
  (:|summary| . "Returns all registered microservice watches.")
  (:|description|
   . "Returns the details of all currently active watches for microservices being monitored by the registry.")
  (:|operationId| . "listAllMicroserviceWatches")
  (:|responses|
   (:|200|
    (:|description|
     . "Success - returns the watches currently in the registry")
    (:|content|
     (:|application/json|
      (:|schema| (:|type| . "array")
       (:|items|
        (:|$ref| . "#/components/schemas/MicroserviceWatch"))))))
   (:|204|
    (:|description|
     . "Success - but there is no content in the registry"))
   (:|400| (:|description| . "Bad request / input format exception"))
   (:|500| (:|description| . "Registry error"))))
 (:|post| (:|tags| "Registry")
  (:|summary|
   . "Registers a 'watch' notification and returns the watch_id")
  (:|description|
   . "Allows clients of the registry to be notified about microservices that match their criteria. This API instructs the registry to 'watch' actions (create, update  and delete) of certain microservices (as defined by the query parameters), and notify the caller when any actions occur with regard to those microservices. The  notifications are sent to the specified notificationEndpoint (defined in the request body) as POST requests and take the form of an array of Notification Schema  Objects.")
  (:|operationId| . "openMicroserviceWatch")
  (:|parameters|
   ((:|name| . "serviceName") (:|in| . "query") (:|required|)
    (:|description|
     . "A regular expression used to match the microservice name.")
    (:|schema| (:|type| . "string")))
   ((:|name| . "serviceDescription") (:|in| . "query") (:|required|)
    (:|description|
     . "A regular expression used to match the microservice description")
    (:|schema| (:|type| . "string")))
   ((:|name| . "serviceTypes") (:|in| . "query") (:|required|)
    (:|description|
     . "A list of the capabilities/intent of the microservice to be matched (one of the types defined by the /schemas/microservices API).  Multiple types can be specified in a comma-separated list (e.g. **serviceTypes=prognosis,estimation** indicates that microservices of type **\"prognosis\"** and **\"estimation\"** would match).")
    (:|schema| (:|type| . "string")))
   ((:|name| . "inputSemantics") (:|in| . "query") (:|required|)
    (:|description|
     . "A list of the desired inputSemantics for a matching service where the inputSemantics are primitives identified in the ontology backed schema (e.g. **inputSemantics=Ports,Cargo** indicates that matching microservices must support either **Ports** or **Cargo** as inputs).")
    (:|schema| (:|type| . "string")))
   ((:|name| . "outputSemantics") (:|in| . "query") (:|required|)
    (:|description|
     . "A list of the desired outputSemantics for a matching service where the outputSemantics are primitives identified in the ontology backed schema (e.g. **outputSemantics=Ports,Cargo** indicates that matching microservices must support either **Ports** or **Cargo** as outputs).")
    (:|schema| (:|type| . "string")))
   ((:|name| . "questionsSupported") (:|in| . "query") (:|required|)
    (:|description|
     . "A list of the types of questions (see the definition of the **\"questionsSupported\"** field in the schema for **POST /microservices** for a list of question types) that must be supported by a matching microservice (e.g. **questionsSupported=Who,What** indicates that matching microservices must support either questions of **Who** or **What**).")
    (:|schema| (:|type| . "string"))))
  (:|responses|
   (:|202|
    (:|description|
     . "Microservice watch registered successfully, and the watch_id is returned")
    (:|content|
     (:|application/json|
      (:|schema| (:|type| . "string") (:|format| . "uuid")))))
   (:|204|
    (:|description|
     . "Success - but there are no microservices which match the query parameters"))
   (:|400| (:|description| . "Bad request / input format exception"))
   (:|500| (:|description| . "Registry error")))
  (:|requestBody|
   (:|content|
    (:|application/json|
     (:|schema|
      (:|properties|
       (:|notificationEndpoint| (:|type| . "string")
        (:|format| . "url")
        (:|description|
         . "The POST endpoint to call to be notified of changes to microservices matching the query parameter constraints."))))))))))

(test test-path-parsing
  (is (equalp '("microservices" "watch" "{watch_id}")
              (split-path (first +path-with+))))
  (is (equalp '("microservices" "watch")
              (split-path (first +path-without+))))
  (is (equalp (list '("microservices" "watch") nil)
              (multiple-value-list (parse-path-parameters (first +path-without+)))))
  (is (equalp (list '("microservices" "watch" "{watch_id}") nil)
              (multiple-value-list (parse-path-parameters (first +path-with+))))))
