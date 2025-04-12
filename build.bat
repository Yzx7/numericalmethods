@echo off
REM ===========================
REM Build script for Windows
REM ===========================

REM 1) Compilar módulos y funciones
echo Compilando módulos y funciones...
gfortran -c 01_bissection.f90
gfortran -c 02_Newton-Raphson.f90
gfortran -c 03_Secante.f90


IF ERRORLEVEL 1 (
  echo Error durante la compilación de módulos/funciones.
  pause
  exit /b 1
)

REM 2) Compilar el programa principal
echo Compilando programa principal...
gfortran -c 00_TestMethods.f90

IF ERRORLEVEL 1 (
  echo Error durante la compilación de 00_TestMethods.f90.
  pause
  exit /b 1
)

REM 3) Enlazar todos los objetos
echo Enlazando en programa.exe...
gfortran 00_TestMethods.o ^
         01_bissection.o ^
         02_Newton-Raphson.o ^
         03_Secante.o ^
         -o programa.exe

IF ERRORLEVEL 1 (
  echo Error durante el enlace.
  pause
  exit /b 1
)

echo ==========================================
echo Compilación y enlace completados con éxito.
echo Ejecutable: programa.exe
echo ==========================================
pause
