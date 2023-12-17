with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Advent_IO; use Advent_IO;

package body Day08 is

type Direction is (Left, Right);
type Key is new String(1..3);
type Part_Enum is (Part_1, Part_2);

package Direction_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Direction);

package Key_Vec is new Ada.Containers.Vectors
  (Index_Type => Natural,
   Element_Type => Key);

package Natural_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Natural);

type Instruction is record
    Left_Opt: Key;
    Right_Opt: Key;
end record;

function Key_Hash(K: Key) return Ada.Containers.Hash_Type is
begin
    return Ada.Strings.Hash(String(K));
end Key_Hash;

package Instruction_Map is new Ada.Containers.Indefinite_Hashed_Maps
  (Key_Type        => Key,
   Element_Type    => Instruction,
   Hash            => Key_Hash,
   Equivalent_Keys => "=");

type Parsed_Input is record
    Directions: Direction_Vec.Vector;
    Mapping: Instruction_Map.Map;
end record;

function Parse_Input(Input: Puzzle_Input) return Parsed_Input is
    Direction_Str: Unbounded_String := Input.First_Element;
    Parsed: Parsed_Input;
    Line: Unbounded_String;
    From: Key;
    Current_Direction: Direction;
    Current_Line: Instruction;
begin
    -- parse directions
    for I in 1..Length(Direction_Str) loop
        Current_Direction := (case Element(Direction_Str, I) is
            when 'L' => Left,
            when 'R' => Right,
            when others => raise Program_Error
        );
        Parsed.Directions.Append(Current_Direction);
    end loop;
    -- parse mapping
    for I in 2..Input.Last_Index loop
        Line := Input(I);
        -- 1234567890123456
        -- AAA = (BBB, CCC)
        From := Key(Slice(Line, 1, 3));
        Current_Line.Left_Opt := Key(Slice(Line, 8, 10));
        Current_Line.Right_Opt := Key(Slice(Line, 13, 15));
        Parsed.Mapping.Insert(From, Current_Line);
    end loop;
    return Parsed;
end Parse_Input;

function Solve(Start_Key: Key; Parsed: Parsed_Input; Part: Part_Enum) return Natural is
    I: Natural := 0;
    Steps: Natural := 0;
    Current_Key: Key := Start_Key;
begin
    loop
        Current_Key := (case Parsed.Directions(I) is
            when Left => Parsed.Mapping(Current_Key).Left_Opt,
            when Right => Parsed.Mapping(Current_Key).Right_Opt
        );       
        Steps := Steps + 1;
        I := I + 1;
        if I = Natural(Parsed.Directions.Length) then
            I := 0;
        end if;
        case Part is
            when Part_1 => exit when Current_Key = "ZZZ";
            when Part_2 => exit when Current_Key(3) = 'Z';
        end case;
    end loop;
    return Steps;
end Solve;

function Part1(Input: Puzzle_Input) return String is
    Parsed: Parsed_Input := Parse_Input(Input);
begin
    return Solve("AAA", Parsed, Part_1)'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Parsed: Parsed_Input := Parse_Input(Input);
    Z_Steps: Natural_Vec.Vector;
    Current_Key: Key;
    Steps: Long_Long_Integer;
    LI: Long_Long_Integer;
    function GCD(A, B: Long_Long_Integer) return Long_Long_Integer is
    begin
        if(B = 0) then
            return A;
        end if;
        return GCD(B, A rem B);
    end GCD;
begin
    -- get keys ending with A and solve until ending with Z
    for K in Parsed.Mapping.Iterate loop
        Current_Key := Instruction_Map.Key(K);
        if Current_Key(3) = 'A' then
            Z_Steps.Append(Solve(Current_Key, Parsed, Part_2));
        end if;
    end loop;
    -- find the lcm of the steps taken to get to keys ending in Z
    Steps := Long_Long_Integer(Z_Steps.First_Element);
    for I of Z_Steps loop
        LI := Long_Long_Integer(I);
        Steps := LI * Steps/GCD(LI, Steps);
    end loop;
    return Steps'Image;
end Part2;

end Day08;
