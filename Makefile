
test:
	erlc rfc6455_client.erl && escript test.erl  ws://localhost:8081/echo/websocket

