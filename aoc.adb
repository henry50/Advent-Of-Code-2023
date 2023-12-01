with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Day01;

procedure AoC is
    -- Get this from cmd line args
    Day_To_Run: Natural := 1;
    -- Get this from a file
    Input: Unbounded_String := To_Unbounded_String("test");
begin
    case Day_To_Run is
        when 1 => 
            Put_Line("Part 1:");
            Put_Line(To_String(Day01.Part1(Input)));
            Put_Line("Part 2:");
            Put_Line(To_String(Day01.Part2(Input)));
        when others =>
            Put_Line("Invalid day");
    end case;
end AoC;