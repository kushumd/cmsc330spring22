class PhoneBook
    def initialize
        @book = Array.new
        @@index = 0
    end

    def add(name, number, is_listed)
        if number.length != 12 || number[3] != "-" || number[7] != "-"
            return false
        end
        for x in 0..number.length - 1
            if x != 3 && x != 7 && number[x] != "1" && number[x] != "2" && number[x] != "3" && number[x] != "4" && number[x] != "5" && number[x] != "6" && number[x] != "7" && number[x] != "8" && number[x] != "9" && number[x] != "0"
                return false
            end
        end
        for person in @book
            if person.getName() == name || (person.getNumber() == number && person.isListed() && is_listed)
                return false
            end
        end
        @book[@@index] = PhoneNumber.new
        @book[@@index].setValues(name, number, is_listed)
        @@index += 1
        return true
    end

    def lookup(name)
        for person in @book
            if person.isListed() && person.getName() == name
                return person.getNumber()
            end
        end
        return nil
    end

    def lookupByNum(number)
        for person in @book
            if person.isListed() && person.getNumber() == number
                return person.getName()
            end
        end
        return nil
    end

    def namesByAc(areacode)
        names = Array.new
        index = 0
        for person in @book
            number = person.getNumber()
            if number[0..2] == areacode
                names[index] = person.getName()
                index += 1
            end
        end
        return names
    end
end

class PhoneNumber
    def initialize
        @name = ""
        @number = ""
        @is_listed = false
    end

    def setValues(name, number, is_listed)
        @name = name
        @number = number
        @is_listed = is_listed
    end

    def getName
        return @name
    end

    def getNumber
        return @number
    end

    def isListed
        return @is_listed
    end
end