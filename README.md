# License Manager Spike

This project is a spike for work to rewrite license_manager. To run it, compile the code, and in an erl shell issue the commands

```
test:start_link(10).
test:go().
```

10 is the number of voice requests to make, and can be adjusted for each test. Progress will be displayed by a series of strings. The characters in the strings have these meanings:

Character | Meaning
---|---
`.` | Nothing has happened yet with this request.
`+` | Request has been granted.
`x` | Request has been denied. Retry will occur in 750 ms.
`-` | License has been released.
`A` | Requested category does not exist.
`B` | Request was already granted.
`C` | Release failed because category does not exist.
`D` | Release "failed" because GUID is not using a license.

  * A can be made to happen if the license file(s) do not contain counts for the voice category.
  * B, C, and D probably won't happen at all in the testing.

Licenses expire after 15 seconds; there is no manual releasing of licenses done in this test code.

While `test:go()` is running, you can call lm directly, via the `lm:request/2` and `lm:release/2` functions. This is one way to cause the B, C, and D statuses above to occur.

## test

* gen_server
* start_links `lm` to maintain a communication link to it.
* API
    * start up with # of requests to make `start_link(Count)` _default: 20_
    * start testing `go()`
* Reports progress of test, visualizing what happens because of the random timing that the test uses.

## lm

* gen_server
* started by `test`
* start_links `license_poller` to maintain a communication link to it.
* API
    * request licenses `request(Category, Giud)`
    * release licenses `release(Category, Guid)`
    * get current state `state()`
* Messages Handled and Responses
    * `state`
    * `{request, Category, Guid}`
        * `no_such_category`
        * `granted`
        * `already_granted`
        * `not_available`
    * `{release, Category, Guid}`
        * `no_such_category`
        * `released`
        * `guid_is_not_using_license`
    * `{expire,  Cagetory, Guid}`
    * `{update_license_info, LicenseFiles}`
* Reading License Files
    * All license files are read, and totals by category are summed up.
    * New max licenses available are stored in the state.

  Responses are also cast back to the parent gen_server (test). This is only for visualizing what's happening.

## license_poller

* gen_server
* started by `lm`
* watches a folder for `*.lic` files
* when a change in files occurs...
    * new file
    * deleted file
    * modified file/timestamp

  sends `{update_license_info, NewFiles}` to its parent (lm).

## License Files

* Can have any name, but must have an extension of `.lic`.
* All files are combined for a total license count.
* Each file contains an erlang term with this definition:
```
{licenses, [{CATEGORY1, COUNT},{CATEGORY2, COUNT},...]}.
```

`CATEGORYn` can be any type of license you intend to control. In the current version, this would be `voice`, `nonvoice`, or `outbound`, but in the future, these may be renamed or discontinued, and others will be added.
