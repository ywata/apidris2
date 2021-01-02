--- http://spec.openapis.org/oas/v3.0.3#info-object
module Language.OpenAPI.Schema

import Data.SortedMap

Map : Type -> Type -> Type
Map = SortedMap



-- 4.7 Schema
-- Optional field will be denoted by Maybe or List type.
--It is possible to define schema without mutual block.
mutual
  ||| OpenAPI Object
  record Object where
    constructor MkObject
    openapi : String --version number
    info : InfoObject
    servers : List ServerObject
    paths : PathsObject
    components : ComponentsObject
    security : List SecurityRequirementObject
    tags : List TagObject
    externalDocs : ExternalDocumentationObject
  
  ||| 4.7.2 Info Object
  record InfoObject where
    title : String
    description : Maybe String
    termsOfService : Maybe String
    contact : Maybe ContactObject
    license : Maybe LicenseObject
    version : String

  ||| 4.7.3 Contact Object
  record ContactObject where
    name : Maybe String
    url : Maybe String
    email : Maybe String

  ||| 4.7.4 License Object
  record LicenseObject where
    name : String
    url : String -- URL

  ||| 4.7.5 Server Object
  record ServerObject where
    url : String -- URL
    description : String
    variables : List (String, ServerVariableObject)

  ||| 4.7.6 Server Variable Object
  record ServerVariableObject where
    enum : [String]
    default' : String
    description : String
    
  ||| 4.7.7 Components Object
  KeyString : Type
  KeyString = String -- that matches "^[a-zA-Z0-9\.\-_]+$"
  record ComponentsObject where
    schema : Maybe (Either (List (String, SchemaObject)) ReferenceObject)
    responses : Maybe (Either (List (String, ResponseObject)) ReferenceObject)
    parameters' : Maybe (Either (List (String, ParameterObject)) ReferenceObject)
    examples : Maybe (Either (List (String, ExampleObject)) ReferenceObject)    
    requestBodies : Maybe (Either (List (String, RequestBodyObject)) ReferenceObject)        
    headers : Maybe (Either (List (String, HeaderObject)) ReferenceObject)            
    securitySchemas : Maybe (Either (List (String, SecuritySchemaObject)) ReferenceObject)            
    links : Maybe (Either (List (String, LinkObject)) ReferenceObject)            
    callbacks : Maybe (Either (List (String, CallbackObject)) ReferenceObject)            
  
  ||| 4.7.8 Paths Object
  record PathObject where
    path : PathItemObject
    
  ||| 4.7.9 Path Item Object
  record PathItemObject where
    ref : String
    summary : Maybe String
    description : Maybe String
    get : Maybe OperationObject
    put : Maybe OperationObject
    post : Maybe OperationObject
    delete : Maybe OperationObject
    options : Maybe OperationObject
    head : Maybe OperationObject
    patch : Maybe OperationObject
    trace : Maybe OperationObject
    servers : List ServerObject
    parameters' : List (Either ParameterObject ReferenceObject)

  ||| 4.7.10 Operation Object
  record OperationObject where
    tags : List String
    summary : Maybe String
    description : Maybe String
    externalDocs : ExternalDocumentationObject
    operationId : String
    parameters' : List (Either ParameterObject ReferenceObject)
    requestBody : List (Either RequestBodyObject ReferenceObject)
    responses : ResponseObject
    callbacks : Map String (Either CallbackObject ReferenceObject)
    deprecated : Boolean
    security : SecurityRequirementObject
    servers : List ServerObject
    
  ||| 4.7.11 External Documentation Object
  record ExternalDocumentationObject where
    description : String
    url : String

  ||| 4.7.12 Parameter Object
  ||| some are skipped here  
  record PrameterObject where
    name : Maybe String
    in' : String
    description : Maybe String
    required : Boolean
    deprecated: Boolean
    allowEmptyValue : Boolean

  ||| 4.7.12.3, 4.7.12.4 are skipped
  ||| 4.7.13 Request Body Object
  record RequestBodyObject where
    description : String
    content : Map String MediaTypeObject
    required : Boolean

  ||| 4.7.14 MediaTypeObject
  record MediaTypeObject where
    schema : Either SchemaObject ReferenceObject
    example : any
    examples : Map String (Either ExampleObject ReferenceObject)
    encoding : Map String EncodingObject

  ||| 4.7.15 EncodingObject
  record EncodingOBject where
    contentType : String
    headers : Map String (Either HeaderOBject ReferenceObject)
    style : String
    explode : Boolean
    allowReserved : Boolean

  ||| 4.7.16 ReponsesObject
  ||| 4.7.16.1 default
  ||| 4.7.16.2 Patterned Field
  ||| 4.7.17 ResponseObject
  record ResponseObject where
    description : String
    headers : Map String (Either HeaderObject ReferenceObject)
    content : Map String MediaTypeObject
    links : Map String (Either LinkObject ReferenceObject)

  ||| 4.7.18 Callback Object
  ||| 4.7.18.1 Patterned Fields
  ||| 4.7.18.2 Key Expression
  
  ||| 4.7.19 ExampleObject
  record ExampleObject where
    summary : String
    description : String
    value: any
    externalValue : String

  ||| 4.7.20 Link Object
  record LinkObject where
    operationRef : String
    operationId : String
    parameters': Map String (Either any expression)
    requestBody : Either any expression
    description String
    server : ServerObject

 ||| 4.7.20.4 Runtime Expression
 ||| 4.7.20.5 Examples
 
 ||| 4.7.21 Header Object

 ||| 4.7.22 TagObject
 record TagObject where
   name : String
   description : String
   externalDocs : ExternalDocumentationObject

   ||| 4.7.23.1, 4.7.23.3, 4.7.23.4

   ||| 4.7.24 Schema Object

   ||| 4.7.25 Discriminator Object
   record DiscriminatorObject where
     propertyName : String
     mapping : Map String String
   ||| XML Object skip
   
   
