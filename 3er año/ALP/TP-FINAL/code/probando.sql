SELECT USER hernan prueba;USE prueba;
SELECT ALL FROM Persona WHERE name LIKE "%"
/*DROP TABLE Trabajador;
DROP TABLE Oficina;
DROP TABLE Persona;
CREATE TABLE Oficina cod Int, KEY cod;
CREATE TABLE Trabajador dni Int, nombre String, cod Int,FOREIGN KEY cod REFERENCES Oficina ON DELETE CASCADES,KEY dni;
CREATE TABLE Persona dni Int,name String,KEY dni;
INSERT Oficina (1),(2),(3);
INSERT Trabajador (1,"Juan",1);
INSERT Trabajador (2,"Juan",2);
INSERT Trabajador (3,"Diego",2);
INSERT Trabajador (4,"Marcelo",3);
INSERT Persona (1,"Diego");
INSERT Persona (2,"Marcelo");
INSERT Persona (3,"Gabriel");
SELECT ALL FROM Trabajador LIMIT 1
SELECT Trabajador.nombre AS name FROM Trabajador,Oficina,Persona
INTERSECT (SELECT nombre FROM Trabajador) (SELECT name   FROM Persona)
DIFF (SELECT nombre FROM Trabajador) (SELECT name   FROM Persona) */
