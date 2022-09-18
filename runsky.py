from sys import stdin, argv

code = stdin if len(argv) < 2 else open(argv[1])


def delay(f):
    done = False

    def closure():
        nonlocal done, f
        if not done:
            done = True
            f = f()
        return f

    return closure


def call(f, *x):
    if len(x) == 1:
        return delay(lambda: f()(x[0])())
    else:
        return call(call(f, *x[:-1]), x[-1])


def function(f, n, *args):
    if n == 0:
        return f(*args)
    else:
        return lambda: lambda x: function(f, n - 1, *args, x)


k = function(lambda x, y: x, 2)
s = function(lambda x, y, z: call(call(x, z), call(y, z)), 3)


def ski():
    token = code.read(1)
    if token == "0":
        return call(ski(), ski())
    elif token == "1":
        return k
    elif token == "2":
        return s


def bit(encoded):
    return call(encoded, lambda: 1, lambda: 0)()


def byte(encoded):
    def decode(*bits):
        byte = 0
        for b in bits:
            byte = byte << 1 | bit(b)
        return lambda: byte

    return call(encoded, function(decode, 8))()


def stream(encoded):
    item = call(
        encoded,
        lambda: None,
        function(lambda x, xs: lambda: (x, xs), 2),
    )()
    if item:
        print(chr(byte(item[0])), end="")
        stream(item[1])


stream(ski())
