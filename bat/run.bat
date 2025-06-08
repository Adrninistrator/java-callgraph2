SETLOCAL ENABLEDELAYEDEXPANSION
set CLASSPATH=
FOR %%C IN (lib\*.jar) DO set CLASSPATH=!CLASSPATH!;%%C
echo %CLASSPATH%
java -Dfile.encoding=UTF-8 -cp .;./jar/run_javacg2.jar;%CLASSPATH%;./config com.adrninistrator.javacg2.entry.JavaCG2Entry

pause...