@echo off
REM Check if Docker is running
docker info >nul 2>&1
if %ERRORLEVEL% neq 0 (
    echo Docker is not running. Please start Docker and try again.
    exit /b 1
)

REM Start the container with the specified command
docker container start -a -i comp6411

REM Capture the exit status of the docker start command
set exitStatus=%ERRORLEVEL%

REM Check if the container started successfully
if %exitStatus% neq 0 (
    echo Failed to start the Docker container comp6411.
    exit /b 1
)

echo Docker container comp6411 started successfully and has now stopped.
