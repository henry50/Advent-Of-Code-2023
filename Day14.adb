with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day14 is

function Part1(Input: Puzzle_Input) return String is
    I, Total: Natural := 0;
    Max_Load: Natural := Natural(Input.Length);
    type Col_Idx is new Natural range 1..Length(Input.First_Element);
    Offset: array(Col_Idx) of Natural := (others => 0);
begin
    for Line of Input loop
        for J in Col_Idx loop
            case Element(Line, Natural(J)) is
                when 'O' =>
                    -- add current load to total
                    Total := Total + (Max_Load - Offset(J));
                    Offset(J) := Offset(J) + 1;
                when '#' =>
                    -- set current offset
                    Offset(J) := I+1;
                when others => null;
            end case;
        end loop;
        I := I + 1;
    end loop;
    return Total'Image;
end Part1;

type Rock_Map is array(Natural range <>, Natural range <>) of Character;

-- Compute one cycle of the rock map
function Cycle(Map: Rock_Map) return Rock_Map is
    subtype Row_Idx is Natural range Map'Range;
    subtype Col_Idx is Natural range Map'Range(1);
    Offset_X: array(Row_Idx) of Col_Idx := (others => 0);
    Offset_Y: array(Col_Idx) of Row_Idx := (others => 0);
    North: Rock_Map(Row_Idx, Col_Idx) := (others => (others => '.'));
    West: Rock_Map(Row_Idx, Col_Idx) := (others => (others => '.'));
    South: Rock_Map(Row_Idx, Col_Idx) := (others => (others => '.'));
    East: Rock_Map(Row_Idx, Col_Idx) := (others => (others => '.'));
begin
    for I in Row_Idx loop
        for J in Col_Idx loop
            case Map(I, J) is
                when 'O' =>
                    North(Offset_Y(J), J) := 'O';
                    if Offset_Y(J) /= Row_Idx'Last then
                        Offset_Y(J) := Offset_Y(J) + 1;
                    end if;
                when '#' =>
                    North(I, J) := '#';
                    if I /= Row_Idx'Last then
                        Offset_Y(J) := I+1;
                    end if;
                when others => null;
            end case;
        end loop;
    end loop;
    for J in Col_Idx loop
        for I in Row_Idx loop
            case North(I, J) is
                when 'O' =>
                    West(I, Offset_X(I)) := 'O';
                    if Offset_X(I) /= Col_Idx'Last then
                        Offset_X(I) := Offset_X(I) + 1;
                    end if;
                when '#' =>
                    West(I, J) := '#';
                    if J /= Col_Idx'Last then
                        Offset_X(I) := J+1;
                    end if;
                when others => null;
            end case;
        end loop;
    end loop;
    Offset_X := (others => Row_Idx'Last);
    Offset_Y := (others => Col_Idx'Last);
    for I in reverse Row_Idx loop
        for J in Col_Idx loop
            case West(I, J) is
                when 'O' =>
                    South(Offset_Y(J), J) := 'O';
                    if Offset_Y(J) /= 0 then
                        Offset_Y(J) := Offset_Y(J) - 1;
                    end if;
                when '#' =>
                    South(I, J) := '#';
                    if I /= 0 then
                        Offset_Y(J) := I-1;
                    end if;
                when others => null;
            end case;
        end loop;
    end loop;
    for J in reverse Col_Idx loop
        for I in Row_Idx loop
            case South(I, J) is
                when 'O' =>
                    East(I, Offset_X(I)) := 'O';
                    if Offset_X(I) /= 0 then
                        Offset_X(I) := Offset_X(I) - 1;
                    end if;
                when '#' =>
                    East(I, J) := '#';
                    if J /= 0 then
                        Offset_X(I) := J-1;
                    end if;
                when others => null;
            end case;
        end loop;
    end loop;
    return East;
end Cycle;

function Part2(Input: Puzzle_Input) return String is
    subtype Row_Idx is Natural range Input.First_Index..Input.Last_Index;
    subtype Col_Idx is Natural range 0..Length(Input.First_Element)-1;
    subtype This_Map is Rock_Map(Row_Idx, Col_Idx);
    package Map_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural,
       Element_Type => This_Map);
    Prev_Results: Map_Vec.Vector;
    Current_Map: This_Map;
    Max_Rank: Natural := Natural(Input.Length);
    Loop_Start: Natural;
    Result_Location: Natural;
    Total: Natural := 0;
begin
    -- parse input
    for I in Row_Idx loop
        for J in Col_Idx loop
            Current_Map(I, J) := Element(Input(Natural(I)), Natural(J)+1);
        end loop;
    end loop;
    -- cycle until the patterns loop
    loop
        Current_Map := Cycle(Current_Map);
        exit when Prev_Results.Contains(Current_Map);
        Prev_Results.Append(Current_Map);
    end loop;
    -- work out the location of the billionth iteration in the cycle
    Loop_Start := Prev_Results.Find_Index(Current_Map);
    Result_Location := Loop_Start + (
        (1_000_000_000 - Loop_Start - 1) rem 
        (Natural(Prev_Results.Length) - Loop_Start)
    );
    Current_Map := Prev_Results(Result_Location);
    -- evaluate map
    for I in Row_Idx loop
        for J in Col_Idx loop
            if Current_Map(I, J) = 'O' then
                Total := Total + Max_Rank - I;
            end if;
        end loop;
    end loop;
    return Total'Image;
end Part2;

end Day14;
