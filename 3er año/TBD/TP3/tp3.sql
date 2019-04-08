
/*Ej1:*/
CREATE TABLE Autor (
    Nombre Char(50),
    Apellido Char(50),
    Nacionalidad Char(50),
    Residencia Char(50),
    Id Integer AUTO_INCREMENT,
    PRIMARY KEY (Id)
);

CREATE TABLE Libro (
    Titulo Char(50),
    Editorial Char(50),
    Precio Int,
    Isbn Char(50),
    PRIMARY KEY (Isbn)
);



CREATE TABLE Escribe (
    Año Date,
    Id Integer,
    Isbn Char(50),
    PRIMARY KEY (Id, Isbn, Año),
    FOREIGN KEY (Id) REFERENCES Autor (Id),
    FOREIGN KEY (Isbn) REFERENCES Libro (Isbn)
    ON DELETE CASCADE
);

/*Ej2:*/
CREATE INDEX titulo_libro
    ON Libro(Título);

CREATE INDEX apellido_autor
    ON Autor(Apellido);

/*Ej3:*/
INSERT INTO Autor (Nombre, Apellido, Nacionalidad, Residencia)
    VALUES ("Damian", "Ariel", "Argentina", "Rosario");
INSERT INTO Autor (Nombre, Apellido, Nacionalidad, Residencia)
    VALUES ("Pablo", "Luis", "Argentina", "Venado");
INSERT INTO Autor (Nombre, Apellido, Nacionalidad, Residencia)
    VALUES ("Abelardo", "Castillo", "Argentina", "Rosario");
INSERT INTO Autor (Nombre, Apellido, Nacionalidad, Residencia)
    VALUES ("Jose", "Saramago", "Portugal", "Lisboa");
INSERT INTO Autor (Nombre, Apellido, Nacionalidad, Residencia)
    VALUES ("Bram", "Stoker", "Inglaterra", "Londres");
INSERT INTO Autor (Nombre, Apellido, Nacionalidad, Residencia)
    VALUES ("Pepito", "Lopes", "Brasil", "San Pablo");


INSERT INTO Libro (ISBN, Titulo, Editorial, Precio)
    VALUES ("0001", "Ensayo Sobre la Ceguera", "EMPA", 800);
INSERT INTO Libro (ISBN, Titulo, Editorial, Precio)
    VALUES ("0002", "Dracula", "EMPA", 800);
INSERT INTO Libro (ISBN, Titulo, Editorial, Precio)
    VALUES ("0003", "Calculo I", "UNR", 300);
INSERT INTO Libro (ISBN, Titulo, Editorial, Precio)
    VALUES ("0004", "Sistemas", "UNR", 100);
INSERT INTO Libro (ISBN, Titulo, Editorial, Precio)
    VALUES ("0005", "Aventuras", "UNR", 100);

INSERT INTO Escribe (Id, Isbn, Año)
    VALUES (4, "0001", '2000-1-1');
INSERT INTO Escribe (Id, Isbn, Año)
    VALUES (5, "0002", '1897-1-1');
INSERT INTO Escribe (Id, Isbn, Año)
    VALUES (5, "0004", '1995-1-1'); 
INSERT INTO Escribe (Id, Isbn, Año)
    VALUES (6, "0005", '1998-1-1'); 



/*Ej4 a:*/
UPDATE Autor SET
    Residencia = "Buenos Aires"
    WHERE Nombre = "Abelardo" AND Apellido = "Castillo";

/*Ej4 b:*/
UPDATE Libro SET
    Precio = Precio + (Precio/10)
    WHERE Editorial = "UNR";

/*Ej4 c:*/
UPDATE Libro INNER JOIN Escribe ON Libro.Isbn = Escribe.Isbn INNER JOIN Autor ON Escribe.Id = Autor.Id SET
    Precio = CASE
                 WHEN (Nacionalidad <> "Argentina" AND Precio < 200) THEN Precio + (Precio/5)
                 WHEN (Nacionalidad <> "Argentina" AND Precio >= 200) THEN Precio + (Precio/10)
             END;


/*Ej4 d:*/
DELETE FROM Libro
	WHERE EXISTS (SELECT * FROM Escribe WHERE Libro.Isbn = Escribe.Isbn AND Año = '1998-1-1');



/*Ej5:*/

/*a*/
SELECT DISTINCT apellido
FROM Persona, PoseeInmueble
WHERE Persona.codigo = PoseeInmueble.codigo_propietario;

/*b*/
SELECT codigo
FROM Inmueble
WHERE precio >= 600000 AND precio <= 800000;

/*c*/
SELECT DISTINCT nombre
FROM Persona,PrefiereZona 
WHERE EXISTS( SELECT * FROM PrefiereZona WHERE  nombre_zona = "Norte" AND nombre_poblacion = "Santa Fe" AND codigo = codigo_cliente)
       AND
       NOT EXISTS (SELECT * FROM PrefiereZona WHERE  (nombre_zona <> "Norte" OR nombre_poblacion <> "Santa Fe") AND codigo = codigo_cliente);

/*d*/
SELECT DISTINCT nombre
FROM Persona JOIN (SELECT vendedor FROM (SELECT codigo FROM Persona JOIN PrefiereZona ON codigo = codigo_cliente WHERE nombre_poblacion = "Rosario" AND nombre_zona = "Centro") AS s1 JOIN Cliente ON s1.codigo = Cliente.codigo ) AS s2 
             ON vendedor = codigo ;


/*e*/
SELECT nombre FROM Persona JOIN (SELECT vendedor FROM Cliente WHERE codigo IN (SELECT vendedor FROM Cliente)) AS s1 ON codigo = vendedor;

/*f*/

SELECT DISTINCT nombre
FROM Persona, (SELECT codigo_cliente
              FROM  PrefiereZona AS S0
              WHERE  NOT EXISTS (SELECT nombre_zona FROM Zona AS S1 WHERE nombre_poblacion = "Rosario"  AND
                     NOT EXISTS (SELECT * FROM PrefiereZona AS S2 WHERE S2.codigo_cliente = S0.codigo_cliente AND  S2.nombre_zona = S1.nombre_zona))) AS S4
WHERE Persona.codigo = S4.codigo_cliente; 




/*g*/
SELECT Persona.nombre, Persona.apellido, Inmueble.codigo, Inmueble.nombre_zona, Inmueble.precio
FROM   Persona, PrefiereZona, Limita, Inmueble
WHERE  Persona.codigo = PrefiereZona.codigo_cliente and /*Zonas preferidas*/
       (Limita.nombre_poblacion = PrefiereZona.nombre_poblacion and /*Zonas limitrofes a las preferidas*/
       Limita.nombre_zona = PrefiereZona.nombre_zona and
       Inmueble.nombre_poblacion = Limita.nombre_poblacion_2 and /*Inmuebles en zonas limitrofes*/
       Inmueble.nombre_zona = Limita.nombre_zona_2 
       or 
       Limita.nombre_poblacion_2 = PrefiereZona.nombre_poblacion and /*Zonas limitrofes a las preferidas*/
       Limita.nombre_zona_2 = PrefiereZona.nombre_zona and
       Inmueble.nombre_poblacion = Limita.nombre_poblacion and /*Inmuebles en zonas limitrofes*/
       Inmueble.nombre_zona = Limita.nombre_zona) 
       and
       Persona.codigo IN /*Chequear que esta persona sea un cliente que visitó todos los inmuebles en sus zonas favoritas*/
       (SELECT codigo FROM Cliente WHERE NOT EXISTS (SELECT * FROM PrefiereZona,Inmueble 
                                                     WHERE Cliente.codigo = PrefiereZona.codigo_cliente and
                                                          Inmueble.nombre_poblacion = PrefiereZona.nombre_poblacion and
                                                          Inmueble.nombre_zona = PrefiereZona.nombre_zona and
                                                          NOT EXISTS (SELECT * FROM Visitas WHERE
                                                                      Visitas.codigo_cliente = Cliente.codigo and
                                                                      Visitas.codigo_inmueble = Inmueble.codigo)));






, Inmueble.codigo, Inmueble.nombre_zona, Inmueble.precio








