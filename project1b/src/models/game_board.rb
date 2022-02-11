class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @ship_board = Array.new(max_row){Array.new(max_column, false)}
        @attack_board = Array.new(max_row){Array.new(max_column, false)}
        @attacks = 0
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        row = ship.start_position.row - 1
        column = ship.start_position.column - 1
        size = ship.size
        orientation = ship.orientation
        if orientation == "Up"
            if row - size < 0
                return false
            end 
            for x in (row - size + 1) .. row 
                if @ship_board[x][column]
                    return false
                end
                @ship_board[x][column] = true
            end
        elsif orientation == "Down"
            if row + size > @max_row
                return false
            end 
            for x in row .. (row + size - 1) 
                if @ship_board[x][column]
                    return false
                end
                @ship_board[x][column] = true
            end
        elsif orientation == "Right"
            if column + size > @max_column
                return false
            end 
            for y in column .. (column + size - 1) 
                if @ship_board[row][y]
                    return false
                end
                @ship_board[row][y] = true
            end
        else 
            if column - size < 0
                return false
            end 
            for y in (column - size + 1) .. column 
                if @ship_board[row][y]
                    return false
                end
                @ship_board[row][y] = true
            end
        end
        return true
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        row = position.row-1
        column = position.column-1
        if row < 0 || row > @max_row || column < 0 || column > @max_column
            return nil
        end
        @attack_board[row][column] = true
        if @ship_board[row][column]
            @attacks += 1
            return true
        end
        return false
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @attacks
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        for x in 0..@max_row - 1 
            for y in  0..@max_column - 1 
                if @ship_board[x][y] && !@attack_board[x][y]
                    return false
                end
            end
        end
        return true
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        
    end
end
