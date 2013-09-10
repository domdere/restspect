# restparse

Parser for a REST API specification language.

## Specification Language Example

The goal is to work towards a human readable specification that also encourages the design of a RESTful web service to follow the
process outlined in Chapter 6 of [**RESTful Web Services**] [restful-web-services-book]

The goal is to parse a specification that looks something like this (human readable enough??):

    `APIName` servesDataTypes
        [   Name : Text
        ,   Age : Int
        ,   Address : Text
        ,   Birthday : DateTime
        ]

    inResources
        (   Person
            [   Name
            ,   Age
            ,   Address
            ]
        )

    viaURIs
        (   GET /person/
            [   Returns : ListOf Person
            ,   Description : "Gets the list of Persons stored on the server"
            ,   Parameters : None
            ,   Errors : None
            ,   Notes : "Nothing."
            ]
        ,   POST /person/
            [   Returns : Person in JSON
            ,   Description : "Allows the Client to push Persons to the server, returns a copy of the stored representation"
            ,   Parameters : None
            ,   Body : Person in JSON
            ,   Errors :
                [   400 : "The Client has sent an incorrect representation."
                ]
            ,   Notes : "Still Nothing."
            ]
        ,   GET /person/{id}
            [   Returns : Person in JSON
            ,   Description : "Retrieves the Person resource with the given id."
            ,   Parameters :
                [   id : "The ID for the desired Person resource"
                ]
            ,   Errors :
                [   404 : "If no Person exists with that ID"
                ]
            ,   Notes : "Nothing Again.."
            ]
        ,   PUT /person/{id}
            [   Returns : Person in JSON
            ,   Description : "Updates the Address or Age for the specified Person"
            ,   Parameters :
                [   id : "The ID for the desired Person resource"
                ]
            ,   Body :
                [   Age
                ,   Address
                ] in JSON
            ,   Errors :
                [   400 : "No Person exists with the given ID"
                ]
            ,   Notes : "Names are not allowed to change."
            ]
        ,   DELETE /person/{id}
            [   Returns : Nothing
            ,   Description : "Deletes a Person"
            ,   Parameters :
                [   id : "The ID for the desired Person resource"
                ]
            ,   Body :
                [   Age
                ,   Address
                ] in JSON
            ,   Errors :
                [   400 : "No Person exists with the given ID"
                ]
            ,   Notes : "Names are not allowed to change."
            ]
        )

Points to note about the above spec:
-   `PersonId` doesnt appear in the data type list, but its there, its an implicit datatype and is understood to be the ID for the `Person` resource,
    It should be understood that a Resources ID doesn't appear in its representations when used as an input.
-   `Body` is an optional Field and can take in an existing Resource definition or a list of Data Types.  If more formats are supported,
    `JSON` could be replaced with something like `[JSON | XML | BSON]`.

-----------------------------------------------------------------------

[restful-web-services]: http://shop.oreilly.com/product/9780596529260.do "RESTful Web Services by Leonard Richardson & Sam Ruby"
