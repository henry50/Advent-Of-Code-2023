with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Advent_IO; use Advent_IO;

package body Day10 is

type Part_Enum is (Part_1, Part_2);

function Solve(Input: Puzzle_Input; Part: Part_Enum) return Integer is
    type Direction is (North, East, South, West);
    type Pipe_Type is (Start, None, NS, EW, NE, NW, SE, SW);
    type Row_Idx is new Natural range Input.First_Index..Input.Last_Index;
    type Col_Idx is new Natural range 0..Length(Input(0))-1;
    type Coord is record
        Row: Row_Idx;
        Col: Col_Idx;
    end record;
    function CoordLt(Left, Right: Coord) return Boolean is 
    begin
        if Left.Row = Right.Row then
            return Left.Col < Right.Col;
        else
            return Left.Row < Right.Row;
        end if;
    end CoordLt;
    function CoordEq(Left, Right: Coord) return Boolean is
    begin
        return Left.Row = Right.Row and Left.Col = Right.Col;
    end CoordEq;
    package Coord_Set is new Ada.Containers.Ordered_Sets
      (Element_Type => Coord,
       "<"          => CoordLt,
       "="          => CoordEq);
    type Input_Grid is array(Row_Idx, Col_Idx) of Pipe_Type;
    Grid: Input_Grid;
    Path: Coord_Set.Set;
    Current_Pos: Coord;
    R: Row_Idx; C: Col_Idx;
    Facing: Direction;
    Intersections: Natural;
    Result: Natural := 0;
    function Now_Facing(D: Direction; P: Pipe_Type) return Direction is
        Invalid_Direction: exception;
        Invalid_Pipe_Type: exception;
    begin
        return (case P is
            when NS => D,
            when EW => D,
            when NE => (case D is
                when South => East,
                when West  => North,
                when others => raise Invalid_Direction
            ),
            when NW => (case D is
                when South => West,
                when East  => North,
                when others => raise Invalid_Direction
            ),
            when SE => (case D is
                when North => East,
                when West  => South,
                when others => raise Invalid_Direction
            ),
            when SW => (case D is
                when North => West,
                when East  => South,
                when others => raise Invalid_Direction
            ),
            when others => raise Invalid_Pipe_Type
        );
    end Now_Facing;
    function Is_Valid(D: Direction; R: Row_Idx; C: Col_Idx) return Boolean is
        P: Pipe_Type;
    begin
        case D is
            when North => 
                if R-1 not in Row_Idx then return False; end if;
                P := Grid(R-1, C);
                return P = NS or P = SW or P = SE;
            when South =>
                if R+1 not in Row_Idx then return False; end if;
                P := Grid(R+1, C);
                return P = NS or P = NW or P = SW;
            when East  =>
                if C+1 not in Col_Idx then return False; end if;
                P := Grid(R, C+1);
                return P = EW or P = NW or P = SW;
            when West  =>
                if C-1 not in Col_Idx then return False; end if;
                P := Grid(R, C-1);
                return P = EW or P = NE or P = SE;
        end case;
    end Is_Valid;
begin
    -- create input grid
    for I in Row_Idx loop
        for J in Col_Idx loop
            Grid(I, J) := (case Element(Input(Natural(I)), Natural(J)+1) is
                when '|' => NS,
                when '-' => EW,
                when 'L' => NE,
                when 'J' => NW,
                when '7' => SW,
                when 'F' => SE,
                when 'S' => Start,
                when others => None
            );
            -- store start position
            if Grid(I, J) = Start then
                R := I; C := J;
            end if;
        end loop;
    end loop;
    Facing := North;
    -- turn until facing a valid connector
    while not Is_Valid(Facing, R, C) loop
        Facing := Direction'Succ(Facing);
    end loop;
    loop
        -- update coordinates
        case Facing is
            when North => R := R - 1;
            when South => R := R + 1;
            when East  => C := C + 1;
            when West  => C := C - 1;
        end case;
        -- add to path
        Current_Pos := (Row => R, Col => C);
        Path.Insert(Current_Pos);
        if Part = Part_1 then
            Result := Result + 1;
        end if;
        -- check for end
        exit when Grid(R, C) = Start;
        -- work out new direction
        Facing := Now_Facing(Facing, Grid(R, C));
    end loop;
    -- end of part 1
    if Part = Part_1 then
        -- half the path length to get the answer
        return Result / 2;
    end if;
    -- use ray casting to determine how many points are inside the path
    for I in Row_Idx loop
        Intersections := 0;
        for J in Col_Idx loop
            Current_Pos := (Row => I, Col => J);
            if Path.Contains(Current_Pos) then
                -- I'm not really sure why ignoring NE and NW works but it does
                if Grid(I, J) /= EW and Grid(I, J) /= NE and Grid(I, J) /= NW then
                    Intersections := Intersections + 1;
                end if;
            -- an odd number of intersections means the point is within the line
            elsif Intersections rem 2 = 1 then
                Result := Result + 1;
            end if;
        end loop;
    end loop;
    return Result;
end Solve;

function Part1(Input: Puzzle_Input) return Integer is
begin
    return Solve(Input, Part_1);
end Part1;

function Part2(Input: Puzzle_Input) return Integer is
begin
    return Solve(Input, Part_2);
end Part2;

end Day10;
