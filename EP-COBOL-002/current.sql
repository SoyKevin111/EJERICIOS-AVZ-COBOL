-- =========================================================
-- SCRIPT DDL + DML - EJERCICIO 4
-- Tablas: FACULTADES, ESTUDIANTES, CONCEPTOS_COBRO
-- =========================================================

-- =========================================================
-- 1. TABLA FACULTADES
-- =========================================================

CREATE TABLE FACULTADES (
    CODIGO   VARCHAR(5)  NOT NULL,
    NOMBRE   VARCHAR(40) NOT NULL,
    ESTADO   CHAR(1)     NOT NULL CHECK (ESTADO IN ('A','I')),
    CONSTRAINT PK_FACULTADES PRIMARY KEY (CODIGO)
);

-- 10 registros de FACULTADES
INSERT INTO FACULTADES (CODIGO, NOMBRE, ESTADO) VALUES
('FAC01', 'INGENIERIA DE SISTEMAS',        'A'),
('FAC02', 'MEDICINA',                       'A'),
('FAC03', 'DERECHO',                        'A'),
('FAC04', 'ADMINISTRACION DE EMPRESAS',     'A'),
('FAC05', 'ARQUITECTURA',                   'A'),
('FAC06', 'PSICOLOGIA',                     'A'),
('FAC07', 'CONTABILIDAD',                   'I'),  -- inactiva (para probar validación 5)
('FAC08', 'INGENIERIA CIVIL',               'A'),
('FAC09', 'COMUNICACION SOCIAL',            'A'),
('FAC10', 'ENFERMERIA',                     'I');  -- inactiva (para probar validación 5)


-- =========================================================
-- 2. TABLA ESTUDIANTES
-- =========================================================

CREATE TABLE ESTUDIANTES (
    CODIGO_ESTUDIANTE  NUMERIC(10) NOT NULL,
    NOMBRE             VARCHAR(40) NOT NULL,
    FACULTAD           VARCHAR(5)  NOT NULL,
    ESTADO             CHAR(1)     NOT NULL CHECK (ESTADO IN ('A','S','R')),
    FECHA_INGRESO      NUMERIC(8)  NOT NULL,   -- formato AAAAMMDD
    VALOR_MATRICULA    NUMERIC(9,2) NOT NULL,
    CONSTRAINT PK_ESTUDIANTES PRIMARY KEY (CODIGO_ESTUDIANTE),
    CONSTRAINT FK_ESTUDIANTES_FACULTAD FOREIGN KEY (FACULTAD)
        REFERENCES FACULTADES (CODIGO)
);

-- 10 registros de ESTUDIANTES
-- (fecha de referencia asumida para pruebas: 20260828)
INSERT INTO ESTUDIANTES (CODIGO_ESTUDIANTE, NOMBRE, FACULTAD, ESTADO, FECHA_INGRESO, VALOR_MATRICULA) VALUES
(1000000001, 'MARIA FERNANDA LOPEZ',    'FAC01', 'A', 20240301, 850.00),
(1000000002, 'CARLOS ANDRES RUIZ',      'FAC02', 'A', 20230915, 1200.00),
(1000000003, 'ANA LUCIA TORRES',        'FAC03', 'S', 20240110, 900.00),  -- suspendida (val. 3)
(1000000004, 'JOSE DAVID MORALES',      'FAC04', 'A', 20260801, 750.00),  -- ingreso reciente (val. 4)
(1000000005, 'PAOLA ESTEFANIA VEGA',    'FAC05', 'A', 20220520, 1000.00),
(1000000006, 'DIEGO ALEJANDRO SOTO',    'FAC06', 'A', 20250612, 650.00),
(1000000007, 'VALERIA NICOLE CASTRO',   'FAC07', 'A', 20230404, 800.00),  -- facultad inactiva (val. 5)
(1000000008, 'LUIS FERNANDO PEÑA',      'FAC08', 'R', 20210228, 950.00),  -- retirado (val. 3)
(1000000009, 'GABRIELA ISABEL ROJAS',   'FAC09', 'A', 20221130, 1100.00),
(1000000010, 'MATEO SEBASTIAN NUÑEZ',   'FAC10', 'A', 20240709, 700.00);  -- facultad inactiva (val. 5)


-- =========================================================
-- 3. TABLA CONCEPTOS_COBRO
-- =========================================================

CREATE TABLE CONCEPTOS_COBRO (
    CODIGO_CONCEPTO  VARCHAR(4)  NOT NULL,
    DESCRIPCION      VARCHAR(30) NOT NULL,
    CENTRO_COSTO     NUMERIC(8)  NOT NULL,
    TIPO_COBRO       CHAR(1)     NOT NULL CHECK (TIPO_COBRO IN ('F','V')),
    ESTADO           CHAR(1)     NOT NULL CHECK (ESTADO IN ('A','I')),
    CONSTRAINT PK_CONCEPTOS_COBRO PRIMARY KEY (CODIGO_CONCEPTO)
);

-- 10 registros de CONCEPTOS_COBRO
INSERT INTO CONCEPTOS_COBRO (CODIGO_CONCEPTO, DESCRIPCION, CENTRO_COSTO, TIPO_COBRO, ESTADO) VALUES
('C001', 'MATRICULA ADICIONAL',       10001, 'V', 'A'),
('C002', 'USO DE LABORATORIO',        10002, 'F', 'A'),
('C003', 'EXAMEN DE REPOSICION',      10003, 'F', 'A'),
('C004', 'CERTIFICADO ACADEMICO',     10004, 'F', 'A'),
('C005', 'CERTIFICADO DE NOTAS',      10004, 'F', 'A'),
('C006', 'CARNET ESTUDIANTIL',        10005, 'F', 'I'),  -- inactivo (val. 7)
('C007', 'DERECHO DE GRADO',          10006, 'V', 'A'),
('C008', 'SEGURO ESTUDIANTIL',        10007, 'F', 'A'),
('C009', 'MULTA BIBLIOTECA',          10008, 'F', 'I'),  -- inactivo (val. 7)
('C010', 'CURSO VACACIONAL',          10009, 'V', 'A');