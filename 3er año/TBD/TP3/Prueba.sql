/*CREATE DATABASE IF NOT EXISTS `Prueba`;
USE `Prueba`;*/



CREATE TABLE `Trabajador`(
  `dni`  INT   NOT NULL, 
  `nombre`      VARCHAR(50) NULL,
  `cod` INT NULL,
  PRIMARY KEY (`dni`),
  FOREIGN KEY (`cod`) REFERENCES `Oficina`(`cod` )  ON DELETE RESTRICT ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


/*CREATE TABLE `Empleado`(
  `dni`  INT   NOT NULL,
  `nombre`      VARCHAR(50) NULL,
  `cod` INT NULL,	
  PRIMARY KEY (`dni`),
  FOREIGN KEY (`cod`) REFERENCES `Oficina`(`cod` )  ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;*/

/*INSERT INTO `Oficina` VALUES(1);
INSERT INTO `Oficina` VALUES(2);
INSERT INTO `Oficina` VALUES(3);
INSERT INTO `Oficina` VALUES(4);*/


INSERT INTO `Oficina` VALUES(2);
INSERT INTO `Trabajador` VALUES(1,"Jose",2);
INSERT INTO `Empleado` VALUES(2,"Diego",2);

/*DELETE FROM `Oficina` WHERE `cod` = 2 ;*/
