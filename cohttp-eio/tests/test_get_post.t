Test GET

  $ test-server &
  $ running_pid=$!
  $ curl -v localhost:8080
    % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                   Dload  Upload   Total   Spent    Left  Speed
    0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0*   Trying ::1:8080...
  * connect to ::1 port 8080 failed: Connection refused
  *   Trying 127.0.0.1:8080...
  * Connected to localhost (127.0.0.1) port 8080 (#0)
  > GET / HTTP/1.1
  > Host: localhost:8080
  > User-Agent: curl/7.76.1
  > Accept: */*
  > 
  * Mark bundle as not supporting multiuse
  < HTTP/1.1 200 OK
  < content-length: 130
  < content-type: text/plain; charset=UTF-8
  < 
  { [130 bytes data]
  100   130  100   130    0     0   126k      0 --:--:-- --:--:-- --:--:--  126k
  * Connection #0 to host localhost left intact
  meth: GET
  resource: /
  version: HTTP/1.1
  headers: Header {
   Accept = "*/*"; User-Agent = "curl/7.76.1"; Host = "localhost:8080" }
  
  $ kill ${running_pid}

Test POST

  $ test-server &
  $ running_pid=$!
  $ curl -v -H "Content-Type: application/json" localhost:8080 --data '{"username":"YOUR.USERNAME","password":"YOUR.PASSWORD"}'
    % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                   Dload  Upload   Total   Spent    Left  Speed
    0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0*   Trying ::1:8080...
  * connect to ::1 port 8080 failed: Connection refused
  *   Trying 127.0.0.1:8080...
  * Connected to localhost (127.0.0.1) port 8080 (#0)
  > POST / HTTP/1.1
  > Host: localhost:8080
  > User-Agent: curl/7.76.1
  > Accept: */*
  > Content-Type: application/json
  > Content-Length: 55
  > 
  } [55 bytes data]
  * Mark bundle as not supporting multiuse
  < HTTP/1.1 200 OK
  < content-length: 245
  < content-type: text/plain; charset=UTF-8
  < 
  { [245 bytes data]
  100   300  100   245  100    55   239k  55000 --:--:-- --:--:-- --:--:--  292k
  * Connection #0 to host localhost left intact
  meth: POST
  resource: /
  version: HTTP/1.1
  headers: Header {
   Content-Length = "55"; Content-Type = "application/json"; Accept = "*/*";
   User-Agent = "curl/7.76.1"; Host = "localhost:8080" }
  
  {"username":"YOUR.USERNAME","password":"YOUR.PASSWORD"}

  $ kill ${running_pid}

