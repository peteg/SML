(* Borrowed from co-http, simplified, clunkified.
   They generated theirs from:
   https://github.com/citricsquid/httpstatus.es

   Licence on the JSON originals:

   Copyright (C) 2012 - 2013 Samuel Ryan (citricsquid)

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*)

(* FIXME
type version = [ `HTTP_1_0 | `HTTP_1_1 | `Other of string ] with sexp

type meth = [ `GET | `POST | `HEAD | `DELETE | `PATCH | `PUT | `OPTIONS | `Other of string ] with sexp

let string_of_version: version -> string = function
  | `HTTP_1_0 -> "HTTP/1.0"
  | `HTTP_1_1 -> "HTTP/1.1"
  | `Other s -> s

let version_of_string: string -> version = function
  | "HTTP/1.0" -> `HTTP_1_0
  | "HTTP/1.1" -> `HTTP_1_1
  | s -> `Other s

let compare_version a b =
  String.compare (string_of_version a) (string_of_version b)

let string_of_method: meth -> string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `PUT -> "PUT"
  | `OPTIONS -> "OPTIONS"
  | `Other s -> s

let method_of_string: string -> meth = function
  | "GET" -> `GET
  | "POST" -> `POST
  | "HEAD" -> `HEAD
  | "DELETE" -> `DELETE
  | "PATCH" -> `PATCH
  | "PUT" -> `PUT
  | "OPTIONS" -> `OPTIONS
  | s -> `Other s

let compare_method a b =
  String.compare (string_of_method a) (string_of_method b)

datatype informational_status
  = Continue
  | Switching_protocols
  | Processing
  | Checkpoint

datatype success_status
  = OK
  | Created
  | Accepted
  | Non_authoritative_information
  | No_content
  | Reset_content
  | Partial_content
  | Multi_status
  | Already_reported
  | Im_used

datatype redirection_status
  = Multiple_choices
  | Moved_permanently
  | Found
  | See_other
  | Not_modified
  | Use_proxy
  | Switch_proxy
  | Temporary_redirect
  | Resume_incomplete

datatype client_error_status
  = Bad_request
  | Unauthorized
  | Payment_required
  | Forbidden
  | Not_found
  | Method_not_allowed
  | Not_acceptable
  | Proxy_authentication_required
  | Request_timeout
  | Conflict
  | Gone
  | Length_required
  | Precondition_failed
  | Request_entity_too_large
  | Request_uri_too_long
  | Unsupported_media_type
  | Requested_range_not_satisfiable
  | Expectation_failed
  | I_m_a_teapot
  | Enhance_your_calm
  | Unprocessable_entity
  | Locked
  | Failed_dependency
  | Upgrade_required
  | Precondition_required
  | Too_many_requests
  | Request_header_fields_too_large
  | No_response
  | Retry_with
  | Blocked_by_windows_parental_controls
  | Wrong_exchange_server
  | Client_closed_request

datatype server_error_status
  = Internal_server_error
  | Not_implemented
  | Bad_gateway
  | Service_unavailable
  | Gateway_timeout
  | Http_version_not_supported
  | Variant_also_negotiates
  | Insufficient_storage
  | Loop_detected
  | Bandwidth_limit_exceeded
  | Not_extended
  | Network_authentication_required
  | Network_read_timeout_error
  | Network_connect_timeout_error

datatype status
  = Informational_Status of informational_status
  | Success_Status of success_status
  | Redirection_Status of redirection_status
  | Client_Error_Status of client_error_status
  | Server_Error_Status of server_error_status

datatype status_code
  = Code of int
  | Status of status
*)

structure Http_codes =
struct

datatype status
  = Code of int

(* informational_status *)
  | Continue
  | Switching_protocols
  | Processing
  | Checkpoint

(* success_status *)
  | OK
  | Created
  | Accepted
  | Non_authoritative_information
  | No_content
  | Reset_content
  | Partial_content
  | Multi_status
  | Already_reported
  | Im_used

(* redirection_status *)
  | Multiple_choices
  | Moved_permanently
  | Found
  | See_other
  | Not_modified
  | Use_proxy
  | Switch_proxy
  | Temporary_redirect
  | Resume_incomplete

(* client_error_status *)
  | Bad_request
  | Unauthorized
  | Payment_required
  | Forbidden
  | Not_found
  | Method_not_allowed
  | Not_acceptable
  | Proxy_authentication_required
  | Request_timeout
  | Conflict
  | Gone
  | Length_required
  | Precondition_failed
  | Request_entity_too_large
  | Request_uri_too_long
  | Unsupported_media_type
  | Requested_range_not_satisfiable
  | Expectation_failed
  | I_m_a_teapot
  | Enhance_your_calm
  | Unprocessable_entity
  | Locked
  | Failed_dependency
  | Upgrade_required
  | Precondition_required
  | Too_many_requests
  | Request_header_fields_too_large
  | No_response
  | Retry_with
  | Blocked_by_windows_parental_controls
  | Wrong_exchange_server
  | Client_closed_request

(* server_error_status *)
  | Internal_server_error
  | Not_implemented
  | Bad_gateway
  | Service_unavailable
  | Gateway_timeout
  | Http_version_not_supported
  | Variant_also_negotiates
  | Insufficient_storage
  | Loop_detected
  | Bandwidth_limit_exceeded
  | Not_extended
  | Network_authentication_required
  | Network_read_timeout_error
  | Network_connect_timeout_error

fun status_of_code (code : int) : status =
    case code of
        100 => Continue
      | 101 => Switching_protocols
      | 102 => Processing
      | 103 => Checkpoint
      | 200 => OK
      | 201 => Created
      | 202 => Accepted
      | 203 => Non_authoritative_information
      | 204 => No_content
      | 205 => Reset_content
      | 206 => Partial_content
      | 207 => Multi_status
      | 208 => Already_reported
      | 226 => Im_used
      | 300 => Multiple_choices
      | 301 => Moved_permanently
      | 302 => Found
      | 303 => See_other
      | 304 => Not_modified
      | 305 => Use_proxy
      | 306 => Switch_proxy
      | 307 => Temporary_redirect
      | 308 => Resume_incomplete
      | 400 => Bad_request
      | 401 => Unauthorized
      | 402 => Payment_required
      | 403 => Forbidden
      | 404 => Not_found
      | 405 => Method_not_allowed
      | 406 => Not_acceptable
      | 407 => Proxy_authentication_required
      | 408 => Request_timeout
      | 409 => Conflict
      | 410 => Gone
      | 411 => Length_required
      | 412 => Precondition_failed
      | 413 => Request_entity_too_large
      | 414 => Request_uri_too_long
      | 415 => Unsupported_media_type
      | 416 => Requested_range_not_satisfiable
      | 417 => Expectation_failed
      | 418 => I_m_a_teapot
      | 420 => Enhance_your_calm
      | 422 => Unprocessable_entity
      | 423 => Locked
      | 424 => Failed_dependency
      | 426 => Upgrade_required
      | 428 => Precondition_required
      | 429 => Too_many_requests
      | 431 => Request_header_fields_too_large
      | 444 => No_response
      | 449 => Retry_with
      | 450 => Blocked_by_windows_parental_controls
      | 451 => Wrong_exchange_server
      | 499 => Client_closed_request
      | 500 => Internal_server_error
      | 501 => Not_implemented
      | 502 => Bad_gateway
      | 503 => Service_unavailable
      | 504 => Gateway_timeout
      | 505 => Http_version_not_supported
      | 506 => Variant_also_negotiates
      | 507 => Insufficient_storage
      | 508 => Loop_detected
      | 509 => Bandwidth_limit_exceeded
      | 510 => Not_extended
      | 511 => Network_authentication_required
      | 598 => Network_read_timeout_error
      | 599 => Network_connect_timeout_error
      | cod => Code cod

fun code_of_status (status : status) : int =
    case status of
        Continue => 100
      | Switching_protocols => 101
      | Processing => 102
      | Checkpoint => 103
      | OK => 200
      | Created => 201
      | Accepted => 202
      | Non_authoritative_information => 203
      | No_content => 204
      | Reset_content => 205
      | Partial_content => 206
      | Multi_status => 207
      | Already_reported => 208
      | Im_used => 226
      | Multiple_choices => 300
      | Moved_permanently => 301
      | Found => 302
      | See_other => 303
      | Not_modified => 304
      | Use_proxy => 305
      | Switch_proxy => 306
      | Temporary_redirect => 307
      | Resume_incomplete => 308
      | Bad_request => 400
      | Unauthorized => 401
      | Payment_required => 402
      | Forbidden => 403
      | Not_found => 404
      | Method_not_allowed => 405
      | Not_acceptable => 406
      | Proxy_authentication_required => 407
      | Request_timeout => 408
      | Conflict => 409
      | Gone => 410
      | Length_required => 411
      | Precondition_failed => 412
      | Request_entity_too_large => 413
      | Request_uri_too_long => 414
      | Unsupported_media_type => 415
      | Requested_range_not_satisfiable => 416
      | Expectation_failed => 417
      | I_m_a_teapot => 418
      | Enhance_your_calm => 420
      | Unprocessable_entity => 422
      | Locked => 423
      | Failed_dependency => 424
      | Upgrade_required => 426
      | Precondition_required => 428
      | Too_many_requests => 429
      | Request_header_fields_too_large => 431
      | No_response => 444
      | Retry_with => 449
      | Blocked_by_windows_parental_controls => 450
      | Wrong_exchange_server => 451
      | Client_closed_request => 499
      | Internal_server_error => 500
      | Not_implemented => 501
      | Bad_gateway => 502
      | Service_unavailable => 503
      | Gateway_timeout => 504
      | Http_version_not_supported => 505
      | Variant_also_negotiates => 506
      | Insufficient_storage => 507
      | Loop_detected => 508
      | Bandwidth_limit_exceeded => 509
      | Not_extended => 510
      | Network_authentication_required => 511
      | Network_read_timeout_error => 598
      | Network_connect_timeout_error => 599
      | Code cod => cod

fun string_of_status (status : status) : string =
    case status of
        Continue => "100 Continue"
      | Switching_protocols => "101 Switching Protocols"
      | Processing => "102 Processing (WebDAV) (RFC 2518)"
      | Checkpoint => "103 Checkpoint"
      | OK => "200 OK"
      | Created => "201 Created"
      | Accepted => "202 Accepted"
      | Non_authoritative_information => "203 Non-Authoritative Information (since HTTP/1.1)"
      | No_content => "204 No Content"
      | Reset_content => "205 Reset Content"
      | Partial_content => "206 Partial Content"
      | Multi_status => "207 Multi-Status (WebDAV) (RFC 4918)"
      | Already_reported => "208 Already Reported (WebDAV) (RFC 5842)"
      | Im_used => "226 IM Used (RFC 3229)"
      | Multiple_choices => "300 Multiple Choices"
      | Moved_permanently => "301 Moved Permanently"
      | Found => "302 Found"
      | See_other => "303 See Other"
      | Not_modified => "304 Not Modified"
      | Use_proxy => "305 Use Proxy (since HTTP/1.1)"
      | Switch_proxy => "306 Switch Proxy"
      | Temporary_redirect => "307 Temporary Redirect (since HTTP/1.1)"
      | Resume_incomplete => "308 Resume Incomplete"
      | Bad_request => "400 Bad Request"
      | Unauthorized => "401 Unauthorized"
      | Payment_required => "402 Payment Required"
      | Forbidden => "403 Forbidden"
      | Not_found => "404 Not Found"
      | Method_not_allowed => "405 Method Not Allowed"
      | Not_acceptable => "406 Not Acceptable"
      | Proxy_authentication_required => "407 Proxy Authentication Required"
      | Request_timeout => "408 Request Timeout"
      | Conflict => "409 Conflict"
      | Gone => "410 Gone"
      | Length_required => "411 Length Required"
      | Precondition_failed => "412 Precondition Failed"
      | Request_entity_too_large => "413 Request Entity Too Large"
      | Request_uri_too_long => "414 Request-URI Too Long"
      | Unsupported_media_type => "415 Unsupported Media Type"
      | Requested_range_not_satisfiable => "416 Requested Range Not Satisfiable"
      | Expectation_failed => "417 Expectation Failed"
      | I_m_a_teapot => "418 I'm a teapot (RFC 2324)"
      | Enhance_your_calm => "420 Enhance Your Calm"
      | Unprocessable_entity => "422 Unprocessable Entity (WebDAV) (RFC 4918)"
      | Locked => "423 Locked (WebDAV) (RFC 4918)"
      | Failed_dependency => "424 Failed Dependency (WebDAV) (RFC 4918)"
      | Upgrade_required => "426 Upgrade Required (RFC 2817)"
      | Precondition_required => "428 Precondition Required"
      | Too_many_requests => "429 Too Many Requests"
      | Request_header_fields_too_large => "431 Request Header Fields Too Large"
      | No_response => "444 No Response"
      | Retry_with => "449 Retry With"
      | Blocked_by_windows_parental_controls => "450 Blocked by Windows Parental Controls"
      | Wrong_exchange_server => "451 Wrong Exchange server"
      | Client_closed_request => "499 Client Closed Request"
      | Internal_server_error => "500 Internal Server Error"
      | Not_implemented => "501 Not Implemented"
      | Bad_gateway => "502 Bad Gateway"
      | Service_unavailable => "503 Service Unavailable"
      | Gateway_timeout => "504 Gateway Timeout"
      | Http_version_not_supported => "505 HTTP Version Not Supported"
      | Variant_also_negotiates => "506 Variant Also Negotiates (RFC 2295)"
      | Insufficient_storage => "507 Insufficient Storage (WebDAV) (RFC 4918)"
      | Loop_detected => "508 Loop Detected (WebDAV) (RFC 5842)"
      | Bandwidth_limit_exceeded => "509 Bandwidth Limit Exceeded (Apache bw/limited extension)"
      | Not_extended => "510 Not Extended (RFC 2774)"
      | Network_authentication_required => "511 Network Authentication Required"
      | Network_read_timeout_error => "598 Network read timeout error"
      | Network_connect_timeout_error => "599 Network connect timeout error"
      | Code cod => Int.toString cod

(* FIXME
let is_informational code =
  match status_of_code code with
  | #informational_status -> true
  |  _ -> false

let is_success code =
  match status_of_code code with
  | #success_status -> true
  |  _ -> false

let is_redirection code =
  match status_of_code code with
  | #redirection_status -> true
  |  _ -> false

let is_client_error code =
  match status_of_code code with
  | #client_error_status -> true
  |  _ -> false

let is_server_error code =
  match status_of_code code with
  | #server_error_status -> true
  |  _ -> false


let is_error code = is_client_error code || is_server_error code
*)

exception Http_server_exception of status

end
