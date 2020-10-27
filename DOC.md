## Decirest code guide

Let's say you would like to create a resource under
**/company/1/** **my_resource**


#### `name/0`
Specify the name of the resource. Will be used to build the paths if `paths/0` is not exported
```
name() ->
  <<"my_resource">>.
```

#### `child_of/0`
Indicate that the resource should be a child of the company resource
```
child_of() ->
  [user_r].
```

#### `ident/0`
Used to specify the identifier used in the callbacks for single handler
```
ident() ->
  my_id.
```
#### `paths/0`   :: optional :: should not be used with ident
Callback to specify paths the resource should handle.
These will be put on top of all parent paths.
For most of the resources the default should be enough and you do not need implement this function.

Default:
```
paths() ->
	[{"/my_resource", decirest_collection_handler},
	{"/my_resource/:my_id", decirest_single_handler}].
```

#### `fetch_data/0`
This is the function where you return data from a `GET`request. This function will also be called whenever someone does a request against any of you children.
single_handler return value http code mapping
```
{ok, [term()]} or term 	200
{ok, [term(), term()] 	409
{ok, []} 				404
{error, Reaon]}			404
```
collection_handler return value http code mapping

The only valida case for collection handler is to return a list of maps.
Decirest will also filter all maps that doesn't have a `data_pk/0` or `id` key

#### `schema/0`
In this function you should provide a json schema so that Decirest can validate the data before providing it to `persist_data/2/3` callback

When you provide a schema Decirest will do 4 callback that you can override
	- `validate_bin/3` here you can validate the binary data. **Default:** nonething
	- `to_term/3` decode binary. **Default:**:  `jiffy:decode(Body, [return_maps])`
	- `validate_on_schema/3` **Default:**: `jesse:validate_with_schema(Schema, Obj, [{allowed_errors, infinity}])`
	- `validate_term/3` here you add extra validation after the schema **Default:** none

#### `persist_data/0`

This is the callback that stores the validated data.
the response Decirest provide depends on the handler:
decirest_collection_handler answers with a redirect `303`to the new resource that was created from the `POST` request
decirest_single_handler answers with a redirect `202` for `PUT`and `PATCH` request

#### `action_schema/0`

In this function you should provide a json schema so that Decirest can validate the data before providing it to `perform_action/2/3` callback. This is only valid for decirest_single_handler.

#### `perform_action/2`   POST

Callback that provides possibility do do a action on the related resource. This function should not be used to change store data mainly but


#### `validate_payload/2`  ** Deprecated**

Old function that requires you to do everything that Decirest does for you by just exporting `schema/0`. `validate_bin/3`, `to_term/3`, `validate_on_schema/3` and `validate_term/3`


# Cowboy callbacks

All callbacks that Decirest provide to Cowboy can be replaced in your own resource. The idea is that these callbacks should not be needed in most cases.

You can override the callbacks in Decirest without having to reimplement the critical parts in the decirest callback by returning run default.
```
init_rest_default(Req, State) ->
  {cowboy_rest, Req#{bindings => decirest_query:get_bindings(Req, State)}, State#{rstate => #{}}}.
```
In you resource you can add something to state and then tell Decirest to call the default callback
```
init(Req, State) ->
	{run_default, [Req, State#{my_resource => something])
```

**Be aware of that if you override some Cowboy callbacks some of your resource callbacks might not be called **

You can get more information on the callbacks in the cowboy documentation.
https://ninenines.eu/docs/en/cowboy/2.6/guide/rest_flowcharts/


## `init/2 `

## `is_authorized/2`

default  ``<< decirest_auth_module >>:authenticate/2``

## `forbidden/2`

calls  `resource_module:gate1`

## `allowed_methods/2`

Decirest will populate allowed_methods based on your callacks plus the defaults:
 - HEAD
 - GET
 -  OPTIONS
#### Single handler:
 - `validate_payload`: PUT, PATCH
 - `delete_data`        : DELETE
 - `action_schema`  : POST
##### Collection handler:
 - `validate_payload` : POST

## `options/2`



## `content_types_accepted/2`

default content types we accept and what callback that will be called for each type
the default callback for handling body in Decirest

```
{{<<"application">>, <<"json">>, '*'}, from_fun},
{{<<"application">>, <<"javascript">>, '*'}, from_fun}
```
####  content type callbacks

 - `from_fun/2` validate & persist callbacks

##  ```content_types_provided/2```
No need to override unless you want to provide something not covered by the default in Decirest
```
{{<<"application">>, <<"json">>, '*'}, to_json},
{{<<"application">>, <<"javascript">>, '*'}, to_json},
{{<<"text">>, <<"html">>, '*'}, to_html},
{{<<"application">>, <<"octet-stream">>, '*'}, to_fun}
```

####  content type callbacks
`to_fun/2` calls `fetch_data/2/3`
`to_json` calls `fetch_data/2/3`
`to_html`calls `fetch_data/2/3`

##  `resource_exists/2`

This is the callback that runs through all ancestors and populate the state with data from them.
