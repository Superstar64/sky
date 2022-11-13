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


def function(f, n, *e):
    if n == 0:
        return f(*e)
    else:
        return lambda: lambda x: function(f, n - 1, *e, x)


def ski(file):
    token = file.read(1)
    if token == "0":
        return call(ski(file), ski(file))
    elif token == "1":
        return function(lambda x, y: x, 2)
    elif token == "2":
        return function(lambda x, y, z: call(call(x, z), call(y, z)), 3)


def byte(encoded):
    def decode(*bits):
        byte = 0
        for b in bits:
            byte = byte << 1 | call(b, lambda: 1, lambda: 0)()
        return lambda: byte

    return call(encoded, function(decode, 8))()


def uncons(encoded):
    return call(encoded, lambda: None, function(lambda x, xs: lambda: (x, xs), 2))()


from sys import stdin, argv

if len(argv) < 2:
    print("Usage: python", argv[0], "input.sky")
    print("Description: Evaluate sky byte code and print the results")
else:
    code = open(argv[1]) if argv[1] != "-" else stdin
    stream = uncons(ski(code))
    while stream:
        print(chr(byte(stream[0])), end="")
        stream = uncons(stream[1])
