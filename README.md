# CL-Swagger #

## Introduction to the Fork

This fork of cl-swagger-codegen was made by [SIFT, LLC](www.sift.net), and maintained [on GitHub](https://github.com/siftech).  We needed to radically extend the capabilities of [incjung's](https://github.com/incjung) [original Common Lisp package](https://github.com/incjung/cl-swagger-codegen). We regret that we do not know incjung's real name, and so cannot give them the full credit that is due.

## Introduction
This project is a lisp code generator that generates templates for clients to OpenAPI-specified REST services.  OpenAPI was previously known as Swagger, and we use the two names interchangeably.

A [Swagger (OpenAPI) code generator](http://swagger.io/swagger-codegen/) is a tool to build a swgger client easier by consuming a Swagger-defined API specification.

There are a number of [code generators](https://github.com/swagger-api/swagger-codegen) for Swagger, but not one for Common Lisp.

With cl-swagger-codegen, you can generate client stub code in Common Lisp.  cl-swagger-codegen employs the following open source CL libraries:
 
 * For creating client stub code:  [cl-mustache](https://github.com/kanru/cl-mustache), a templating library.
 
 * For HTTP communication: [drakma](https://github.com/edicl/drakma), a CL HTTP/HTTPS client.
 
 * For JSON: [cl-json](https://github.com/hankhero/cl-json),  a  JSON encoder/decoder.
 
 At runtime, the generated clients depend on drakma and cl-json, which must be loaded.

## Install/Build
This forked version of `cl-swagger-codegen` *cannot* be installed using Quicklisp. 

- Download the source code from [this git repo](https://github.com/siftech/cl-swagger-codegen).

- Build and load in a Common Lisp REPL:

```
 (asdf:load-system "cl-swagger")
```


## How to generate client Code with cl-swagger ##


### Generating client lisp code from swagger url ###

Let's take an example.:

"http://petstore.swagger.io/" is a sample Petstore server that is like "hello world" for Swagger.

You can generate a client stub code in `pet-api-client.lisp`  in the current working directory as follows:

```
(generate-client "http://petstore.swagger.io/v2/swagger.json" #p"./example/pet-api-client.lisp")
```

If you open the generated `pet-api-client.lisp` you will find `rest-call` and `pets-pet` functions:

    (defun rest-call (....
      "call http-request with basic params and conteent and authorization"
      .....)

    ...

    ;;
    ;; 
    ;; * path-url : /pet
    ;;
    (defun post-pet (&key params content basic-authorization)
      (rest-call "http://petstore.swagger.io/v2" "/pet" :params params :content content
                                :basic-authorization basic-authorization
                                :method :post
                                :accept "application/json"
                                :content-type "application/json"))

    ...


`rest-call` is the core function for HTTP communication. This function will always be created by cl-swagger-gen. The remaining functions wrap the `rest-call` for convenience.  For example,  `post-pet` is a wrapper function to access `POST /PET/` 


### Generating client lisp code directly from Swagger JSON file ###

Without a running server providing the Swagger JSON, you can create a client code from a Swagger JSON FILE.
Assuming you have a Swagger file, for example, `swagger.json`, you can generate client code as follows:

```
(generate-client #p"./directory-path/swagger.json" "client2.lisp")
```

Note that the first argument *must be a Common Lisp pathname* and *not* a namestring.  This should probably be fixed.

## How to use the client code created by cl-swagger ##

**NOTE:** The following have not been updated for the fork.

### Example 1: Petstore
 
- [BASIC-TUTORIAL](https://github.com/incjung/cl-swagger-codegen/wiki/Tutorial1_Basic)
 
#### Create Client Code
```
(generate-client "http://petstore.swagger.io/v2/swagger.json" #p"./example/pet-api-client.lisp")
```

#### Usage 
Find the `pet-api-client.lisp` and load the file. 

To add a new pet to the online store, use `POST-PET`. We use [`cl-json`](https://common-lisp.net/project/cl-json/) to manipulate JSON structures. 


    (post-pet 
     :content (cl-json:encode-json-to-string '((id . 0)
                                               (:category . ((:id . 0) (:name . "string")))
                                               (:name . "doggie")
                                               ("photoUrls" . #("string"))
                                               (:tags . (((:id . 0)
                                                          (:name . "string"))))
                                               (:status . "available"))))


This has the same effect as the following `curl` command:

```
curl -X POST "http://petstore.swagger.io/v2/pet" -H "accept: application/xml" -H "Content-Type: application/json" -d "{ \"id\": 0, \"category\": { \"id\": 0, \"name\": \"string\" }, \"name\": \"doggie\", \"photoUrls\": [ \"string\" ], \"tags\": [ { \"id\": 0, \"name\": \"string\" } ], \"status\": \"available\"}"
```

### Example 2: Google URL Shortener Service

Please visit the api explorer (https://developers.google.com/apis-explorer/#p/urlshortener/v1/).

For swagger file, you can find open-api swagger at `https://github.com/APIs-guru/openapi-directory/tree/master/APIs/googleapis.com`. Inthe case that the swagger is not a json format, I just used `https://www.browserling.com/tools/yaml-to-json` to save the `swagger.json`.


#### Create Client Code

    (generate-client #p"./example/urlshortener.json" #p"./example/urlshortener-api-client.lisp")


#### Useage 
Find the `urlshortener-api-client.lisp` and load the file. 

To expand a shortened URL, use `get-url`.

    (get-url :params '(("key" . "XXXXX-YOUR-KEY-XXXXXXXXXXXX")
                       ("shortUrl" . "https://goo.gl/fbsS")))


This is same with `curl` command:
```
;;  curl -X GET "https://www.googleapis.com/urlshortener/v1/url?shortUrl=https%3A%2F%2Fgoo.gl%2FfbsS&key=XXXXXXX" -H "accept: application/json"
```

#### API KEY
To acquire and use your API key, visit "https://developers.google.com/url-shortener/v1/getting_started" and get API key


### Example 3: Google Calendar Service
Please visit [Google's API explorer](https://developers.google.com/apis-explorer/#p/calendar/v3/)

For Swagger file, you can find open-api swagger at `https://github.com/APIs-guru/openapi-directory/tree/master/APIs/googleapis.com`. In the case that the swagger is not a json format, I just used `https://www.browserling.com/tools/yaml-to-json` to save the `swagger.json`.

#### Create Client Code

    (generate-client #p"./example/cal-swagger.json" #p"./example/cal-api-client.lisp")


#### Usage 
Find the `cal-api-client.lisp` and load the file. 

To get my scheduler calendars, use `get-users-me-calendarlist` function. 

    (get-users-me-calendarlist :params `(("access_token" . ,access-token)))


`access_token` came from Google Oauth2 process. You shoud have google account for Google services if you want to get your token. 

### Misc 
#### BASIC AUTHORIZATON
When the rest api require basic authorization, you can use. `:basic-authorization`


    (<function-Name> :basic-authorization '("id" . "password"))


#### OAUTH2 AUTHORIZATON
For OAuth2, use *access-token* with *:params*

    (<function-Name> :params '(("access-token" "-your-key-")))


for example:


    (defparameter access-token "XXXXXXX YOUR-TOKEN-KEN XXXXXXXX")

    (get-users-me-calendarlist :params `(("access_token" . ,access-token)))

For more information and exaple, please read my [wiki](https://github.com/incjung/cl-swagger-codegen/wiki/Google-Oauth2-Process)

#### JSON Structure
For json object, cl-swagger-codegen uses `cl-json`. 

    (cl-json:encode-json-to-string '((id . 0)
                                    (:category . ((:id . 0) (:name . "string")))
                                    (:name . "doggie")
                                    ("photoUrls" . #("string"))
                                    (:tags . (((:id . 0)
                                    (:name . "string"))))
                                    (:status . "available")))


for JSON object:

    {
      "id": 0,
      "category": {
        "id": 0,
        "name": "string"
      },
      "name": "doggie",
      "photoUrls": [
        "string"
      ],
      "tags": [
        {
          "id": 0,
          "name": "string"
        }
      ],
      "status": "available"
    }
