
--Ej 1:
type Color = (Float,Float,Float)

merge::Color->Color->Color
merge (x1,y1,z1) (x2,y2,z2) = ((x1+x2)/2,(y1+y2)/2,(z1+z2)/2)


--Ej 2:

type Line = (String,Int)

empty::Line
empty= ("",0)


moveLeft::Line->Line
moveLeft (line,pos) |pos > 0 = (line,pos-1) 
					|otherwise = (line,pos)

moveRight::Line->Line
moveRight (line,pos)| pos+1 > (length line) = (line,pos)
					| otherwise = (line,pos+1)

moveStart::Line->Line
moveStart (line,pos) = (line,0)

moveEnd::Line->Line
moveEnd (line,pos) = (line,(length line))


insert::Line->Char->Line
insert (line,pos) char = ((take (pos-1) line)++[char]++(drop (pos-1) line),pos+1)

delete::Line->Line
delete (line,pos) | line == "" = (line,pos)
				  |otherwise = ((take (pos-1) line)++(drop pos line),pos-1)
