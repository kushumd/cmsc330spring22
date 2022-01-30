def fib(n)
    a = Array.new
    if n > 0
        a[0] = 0
    end
    if n > 1
        a[1] = 1
    end
    for x in 2..n-1
        a[x] = a[x-1] + a[x-2]
    end
    return a
end

def isPalindrome(n)
    s = sprintf("%d", n)
    for x in 0..s.length/2
        if s[x] != s[s.length-1-x]
            return false
        end
    end
    return true
end

def nthmax(n, a)
    if n > a.length - 1
        return nil
    end
    max = 0
    for x in 0..n
        max = 0
        for x in a
            if x > max
                max = x
            end
        end
        a.delete(max)
    end
    return max
end

def freq(s)
    if s.length == 0
        return ""
    end
    count = Hash.new(0)
    for x in 0..s.length
        count[s[x]] = count[s[x]] + 1
    end
    max = 0
    str = ""
    for x in count.keys()
        if count[x] > max
            max = count[x]
            str = x
        end
    end
    return str
end

def zipHash(arr1, arr2)
    if arr1.length != arr2.length
        return nil
    end
    corr = Hash.new
    for x in 0..arr1.length-1
        corr[arr1[x]] = arr2[x]
    end
    return corr
end

def hashToArray(hash)
    arr = Array.new
    index = 0
    for key in hash.keys()
        arr[index] = [key, hash[key]]
        index = index + 1
    end
    return arr
end
