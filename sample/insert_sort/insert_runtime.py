k = lambda x: lambda y: x
s = lambda x: lambda y: lambda z: x(z)(y(z))

def swapi(array,i,j):
	x = array[i]
	y = array[j]
	array[i] = y
	array[j] = x
	return array

whilef = lambda state: lambda cond: lambda modify: whilef (modify(state))(cond)(modify) if cond(state) else state
view = lambda state: lambda f: f(state[0])(state[1])(state[2])
pack = lambda a: lambda b: lambda c: [a,b,c]
zero = 0
minus1 = lambda x: x - 1
plus1 = lambda x: x + 1
lessthan = lambda x: lambda y: x < y
greater = lambda x: lambda y: x > y
andf = lambda x: lambda y: x and y
swap = lambda array: lambda i: lambda j: swapi(array.copy(),i,j)
index = lambda array: lambda i: array[i]
length = lambda array: len(array)

insertSortF = TERM

insertSort = insertSortF(whilef)(view)(pack)(zero)(minus1)(plus1)(lessthan)(greater)(andf)(swap)(index)(length)

print(insertSort([2,1,3,0,5,0]))
