require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    ship_count = 0
    GameBoard.new(10, 10)
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    attacks = Array.new
    lines = path.split("\n")
    index = 0
    for line in lines
        line =~ /\(\d, \d\)$/
        attacks[index] = Position.new($1, $2)
        index += 1
    end
    return attacks
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
