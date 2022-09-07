import redis
import sys
r = redis.Redis()
n = 0
dir = sys.argv[1]
for key in r.keys():
  try:
    val = r.hgetall(key)[b'result']
    if val != b"":
      s = key.decode('utf-8') + val.decode('utf-8')
      f = open(dir + "/" + str(n) + '.opt', "w")
      n = n + 1
      f.write(s)
      f.close()
  except KeyError:
    pass
