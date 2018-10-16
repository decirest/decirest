Example app
===

An example app showcasing decirests different functionalities, and base
for testing.

Tests
---
some test functions to run:

```
curl -v -H"content-type: application/json"  -X POST -d '{"id": 34, "name":"jsopost", "description": "testing post"}'  http://127.0.0.1:8081/bo/account/
curl -v -H"content-type: application/json"  -X PUT -d '{"id": 1, "name":"jso", "descriptions": "put seems to work"}'  http://127.0.0.1:8081/bo/account/1


```
