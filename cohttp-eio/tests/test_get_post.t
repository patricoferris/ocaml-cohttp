Test GET success.

  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > GET /get HTTP/1.1
  > 
  > EOF
  HTTP/1.1 200 OK
  content-length: 63
  content-type: text/plain; charset=UTF-8
  
  meth: GET
  resource: /get
  version: HTTP/1.1
  headers: Header {  }
  $ kill ${running_pid}

Test GET error.
The test should respond with error message since we are trying to read request body. HTTP 1.1 doesn't support request bodies in GET.

  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > GET /get_error HTTP/1.1
  > 
  > EOF
  HTTP/1.1 200 OK
  content-length: 4
  content-type: text/plain; charset=UTF-8
  
  PASS
  $ kill ${running_pid}

Test POST

  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > POST /post HTTP/1.0
  > Content-Length:12
  > 
  > hello world!
  > EOF
  HTTP/1.1 200 OK
  content-length: 100
  content-type: text/plain; charset=UTF-8
  
  meth: POST
  resource: /post
  version: HTTP/1.0
  headers: Header { Content-Length = "12" }
  
  hello world!
  $ kill ${running_pid}
