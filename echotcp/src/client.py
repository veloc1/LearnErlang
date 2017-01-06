import socket

s = socket.socket()
s.connect(("localhost", 4802))
data = s.recv(4096)
print(data)
s.close()
