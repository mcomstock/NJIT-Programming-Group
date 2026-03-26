function main()
    a = 100.0f0
    b = 0.0f0

    for i = 1:1000
        a += 0.001f0
        b += 0.001f0
    end

    println(a)
    println(100.0f0 + b)
    println(100.0f0 +
            1000.0f0 * 0.0010f0)

    println()

    println((1.0f0 / 3.0f0) * 5.0f0)
    println(5.0f0 / 3.0f0)

    println()

    a = 70.00007f0
    b = a*a
    b = b*b
    b = b*b
    println(a*a*a*a*a*a*a*a)
    println(b)
    println(a^8)

    println()

    x = 100.0f0 + 0.001f0
    y = 100.0f0 - 0.001f0

    println(x*x - y*y)
    println((x+y) * (x-y))
end

main()
