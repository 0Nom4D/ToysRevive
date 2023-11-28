*** Settings ***
Documentation       Tests of the / route.
...                 Ensures that "Hello World" works corectly.

Resource            ../rest.resource

*** Test Cases ***
Hello World
    [Documentation]    Get a "Hello world"
    &{get}=    GET    /
    Output
    Integer    response status    200
	Should Be Equal As Strings   ${get.body}    {'message': 'Hello World'}