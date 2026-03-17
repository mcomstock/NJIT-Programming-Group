a = 0

def add1(x):
    x = x + 1

add1(a)
print(a)

b = [0, 1, 2]

def change_first(x):
    x[0] = 3

change_first(b)
print(b)

c = [0, 1, 2]

def new_list(x):
    x = [3, 4]

new_list(c)
print(c)

d = [0, 1, 2]

add1(d[0])
print(d)

e = 0

def print_e():
    print(e)

e = 1
print_e()

def get_print_f():
    f = 0

    def print_f():
        print(f)

    return print_f

my_print_f = get_print_f()
my_print_f()

f = 1
my_print_f()

def get_print_g():
    g = 0

    def print_g():
        print(g)

    g = 1

    return print_g

my_print_g = get_print_g()
my_print_g()

def get_print_h(x):
    h = x

    def print_h():
        print(h)

    return print_h

my_print_h_0 = get_print_h(0)
my_print_h_1 = get_print_h(1)

my_print_h_0()
my_print_h_1()

def get_print_i():
    i = 0

    def print_i():
        print(i)

    return print_i, i

my_print_i, my_i = get_print_i()
my_i = 1
my_print_i()

def get_print_j():
    j = [0, 1, 2]

    def print_j():
        print(j)

    return print_j, j

my_print_j, my_j = get_print_j()
my_j[0] = 3
my_print_j()
