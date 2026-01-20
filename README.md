# COMP 6411: Programming and Data Structures
## Summer 2024 - Concordia University, Montreal, Quebec

---

## Table of Contents
1. [Overview](#overview)
2. [Repository Structure](#repository-structure)
3. [System Requirements](#system-requirements)
4. [Docker Environment Setup](#docker-environment-setup)
5. [Assignment 1: TCP Client-Server Database System](#assignment-1-tcp-client-server-database-system)
6. [Assignment 2: Clojure Sales Data Management](#assignment-2-clojure-sales-data-management)
7. [Project: Distributed Rock-Paper-Scissors Tournament](#project-distributed-rock-paper-scissors-tournament)
8. [Architecture Diagrams](#architecture-diagrams)
9. [Usage Instructions](#usage-instructions)
10. [Testing](#testing)
11. [Design Decisions](#design-decisions)
12. [Troubleshooting](#troubleshooting)

---

## Overview

This repository contains all coursework for **COMP 6411: Programming and Data Structures** taken during Summer 2024. The course explores advanced programming concepts, concurrent programming, and distributed systems through practical implementations in multiple programming languages including Python, Clojure, and Erlang.

### Key Concepts Covered:
- Concurrent and parallel programming
- Client-server architecture
- Functional programming (Clojure)
- Actor model (Erlang)
- Process synchronization
- Distributed systems design
- Data structure implementations

---

## Repository Structure

```
COMP6411-SUMMER2024/
│
├── README.md                    # This comprehensive documentation
├── Dockerfile                   # Docker environment configuration
├── ContainerRunner.bat          # Windows Docker container runner
├── ContainerRunner.sh           # Linux/Mac Docker container runner
│
├── game.erl                     # Original game implementation
├── player.erl                   # Original player implementation
│
├── AS1/                         # Assignment 1: TCP Client-Server System
│   ├── client.py               # Client implementation
│   ├── server.py               # Server implementation
│   └── data.txt               # Sample customer database
│
├── AS2/                         # Assignment 2: Clojure Sales System
│   ├── menu.clj               # Main menu interface
│   ├── db.clj                 # Database operations
│   ├── cust.txt               # Customer data
│   ├── prod.txt               # Product data
│   └── sales.txt              # Sales transactions
│
└── Project/                     # Final Project: RPS Tournament
    ├── README.txt             # Project information
    ├── game.erl               # Enhanced game server
    ├── player.erl             # Enhanced player processes
    └── p1.txt                 # Sample player data
```

---

## System Requirements

### Minimum Requirements:
- **Operating System**: Windows 10+, macOS 10.14+, or Linux (Ubuntu 18.04+)
- **Docker**: Version 20.10.0+
- **RAM**: 4GB minimum, 8GB recommended
- **Storage**: 2GB free space

### Software Dependencies:
- Docker Engine
- Python 3.8+ (for AS1)
- Erlang/OTP 24+ (for game implementations)
- Clojure 1.11+ (for AS2)
- Leiningen (for Clojure project management)

---

## Docker Environment Setup

### Quick Start:
```bash
# Build the Docker image
docker build -t comp6411 .

# Create and run container
docker run -it --name comp6411 -v $(pwd):/workspace comp6411
```

### ContainerRunner Scripts:
- **Windows**: Double-click `ContainerRunner.bat` or run in Command Prompt
- **Linux/Mac**: Execute `./ContainerRunner.sh` in terminal

### Dockerfile Contents:
```dockerfile
FROM gcc  # Base image with GCC compiler

# Install essential tools and languages:
# - Development tools (cmake, valgrind, scons)
# - Text editor (nano)
# - Clojure and Leiningen
# - Erlang and Rebar3
```

---

## Assignment 1: TCP Client-Server Database System

### Architecture Overview
```
┌─────────────────┐     TCP/IP     ┌─────────────────┐
│   Client.py     │ ─────────────▶ │   Server.py    │
│   (Customer     │    Port 9999   │   (Database    │
│    Interface)   │ ◀───────────── │    Manager)    │
└─────────────────┘                └─────────────────┘
         │                                    │
         ▼                                    ▼
   User Input                           File I/O
   Validation                        (data.txt)
```

### Features:
1. **CRUD Operations**:
   - Find, Add, Delete, Update customers
   - Field validation (name, age, address, phone)
   
2. **Data Validation**:
   ```python
   # Name: Alphabetic characters only
   # Age: 1-120 (optional)
   # Address: Alphanumeric with spaces, periods, dashes (optional)
   # Phone: XXX-XXXX or XXX XXX-XXXX format (optional)
   ```

3. **Protocol Design**:
   - Pipe-separated command format: `command|param1|param2|...`
   - Status codes: 200 (OK), 404 (Not Found), 400 (Bad Request), 409 (Conflict)

### Running Assignment 1:
```bash
# Terminal 1: Start server
cd AS1
python server.py

# Terminal 2: Run client
python client.py
```

---

## Assignment 2: Clojure Sales Data Management

### System Architecture
```
┌─────────────────────────────────────────────┐
│              Menu System                    │
│  (menu.clj)                                │
│  • User interface                          │
│  • Option routing                          │
└───────────────┬─────────────────────────────┘
                │
                ▼
┌─────────────────────────────────────────────┐
│            Database Layer                   │
│  (db.clj)                                  │
│  • Data loading & parsing                  │
│  • Business logic                          │
│  • Table joins & calculations              │
└───────────────┬─────────────────────────────┘
                │
        ┌───────┴───────┐
        ▼               ▼
┌─────────────┐ ┌─────────────┐ ┌─────────────┐
│ cust.txt    │ │ prod.txt    │ │ sales.txt   │
│ Customers   │ │ Products    │ │ Transactions│
└─────────────┘ └─────────────┘ └─────────────┘
```

### Data Model:
```clojure
;; Customer Table: {ID -> [Name Address Phone]}
{1 ["John Smith" "123 Here Street" "456-4567"]}

;; Product Table: {ID -> [Description UnitCost]}
{1 ["shoes" 14.96]}

;; Sales Table: [[SaleID [CustID ProdID Quantity]]]
[[1 [1 1 3]]]  ;; Sale #1: Customer 1 bought 3 of product 1
```

### Features:
1. **Interactive Menu System** with clear screen functionality
2. **Recursive data processing** without loops
3. **Relational operations**:
   - Customer-product joins
   - Total sales calculations
   - Inventory count aggregation
4. **Immutable data structures** ensuring thread safety

### Running Assignment 2:
```bash
cd AS2
clojure -M -m menu
# OR using Leiningen
lein run
```

---

## Project: Distributed Rock-Paper-Scissors Tournament

### System Architecture
```
┌─────────────────────────────────────────────────────┐
│                  Game Coordinator                   │
│                (game.erl - Master)                  │
│  • Tournament management                            │
│  • Game scheduling & result processing              │
│  • Player state tracking                            │
└─────────────────┬───────────────────────────────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
    ▼             ▼             ▼
┌─────────┐ ┌─────────┐ ┌─────────┐
│ Player1 │ │ Player2 │ │ PlayerN │
│ Process │ │ Process │ │ Process │
│ (Actor) │ │ (Actor) │ │ (Actor) │
└─────────┘ └─────────┘ └─────────┘
    │           │           │
    └───────────┼───────────┘
                │
                ▼
           Random Move
          Generation
```

### Process Communication Flow:
```
1. Player Process          2. Game Coordinator       3. Opponent Process
      │                            │                         │
      │──{play_game, Opponent}───▶│                         │
      │                            │──{invitation}─────────▶│
      │                            │◀─{game_request}───────│
      │◀─{play_game_with_gameid}──│                         │
      │                            │◀─{play_game_with_gameid}
      │──{match_move}────────────▶│                         │
      │                            │                         │──{match_move}─▶
      │                            │◀─{match_move}──────────│
      │◀─{main_result}────────────│                         │
      │                            │                         │◀─{main_result}
```

### Enhanced Features (Project vs Original):
| Feature | Original Implementation | Project Implementation |
|---------|------------------------|------------------------|
| Move History | No tracking | Full move sequence tracking |
| Credit System | Basic deduction | Comprehensive credit management |
| Disqualification | Immediate removal | Backlog tracking |
| Game Ties | Simple replay | Strategic retry with history |
| Reporting | Basic results | Detailed tournament analytics |

### Game Rules Implementation:
```erlang
% Win conditions matrix
determine_winner(rock, scissors) -> win;
determine_winner(scissors, paper) -> win;
determine_winner(paper, rock) -> win;
determine_winner(scissors, rock) -> lost;
determine_winner(paper, scissors) -> lost;
determine_winner(rock, paper) -> lost;
determine_winner(_, _) -> tie.
```

### Tournament Lifecycle:
1. **Initialization**: Load players from file (e.g., `p1.txt`)
2. **Matchmaking**: Random pairing with credit checks
3. **Game Execution**: Concurrent move processing
4. **Result Processing**: Credit updates and disqualifications
5. **Tournament Conclusion**: Winner declaration with statistics

### Running the Project:
```erlang
# Compile the modules
erlc game.erl player.erl

# Start the tournament with player data
erl -noshell -s game start p1.txt -s init stop

# Interactive Erlang shell alternative
erl
1> c(game), c(player).
2> game:start(["p1.txt"]).
```

---

## Architecture Diagrams

### 1. Complete System Overview
```
┌─────────────────────────────────────────────────────────────┐
│                    COMP 6411 ECOSYSTEM                      │
├─────────────┬────────────────┬──────────────────────────────┤
│   AS1       │      AS2       │          Project             │
│ Python      │   Clojure      │         Erlang/OTP           │
│ TCP/IP      │  Functional    │   Actor Model & Concurrency  │
│ Client-     │  Programming   │   Distributed Tournament     │
│ Server      │  Data          │                              │
│             │  Processing    │                              │
└─────────────┴────────────────┴──────────────────────────────┘
         │               │                        │
         └───────────────┼────────────────────────┘
                         │
                  ┌──────▼──────┐
                  │  Docker     │
                  │  Container  │
                  │  Environment│
                  └─────────────┘
```

### 2. Actor Model in Erlang Project
```
┌─────────────────────────────────────────────────────────┐
│                    Supervisor/Game                      │
├─────────────────────────────────────────────────────────┤
│  PID: <0.1.0>                                           │
│  State: TournamentData, PlayerList, GameHistory        │
│  Mailbox:                                               │
│    • {game_request, P1, P2}                            │
│    • {match_move, GameID, Move}                        │
│    • {player_disqualified, Name}                       │
└──────────────┬──────────────────────────────────────────┘
               │ Spawns/Monitors
    ┌──────────┴──────────┐
    ▼                     ▼
┌─────────┐         ┌─────────┐
│ Player  │         │ Player  │
│ Actor   │         │ Actor   │
├─────────┤         ├─────────┤
│ State:  │         │ State:  │
│ • Name  │         │ • Name  │
│ • Credits│         │ • Credits│
│ • Strategy│        │ • Strategy│
└─────────┘         └─────────┘
```

### 3. Data Flow in Clojure Assignment
```
       ┌─────────────┐
       │   Text      │
       │   Files     │
       │ (cust.txt,  │
       │  prod.txt,  │
       │  sales.txt) │
       └──────┬──────┘
              │ Load & Parse
       ┌──────▼──────┐
       │ Immutable   │
       │ Data        │
       │ Structures  │
       │ (Maps,      │
       │  Vectors)   │
       └──────┬──────┘
              │ Transform
       ┌──────▼──────┐
       │ Business    │
       │ Logic       │
       │ • Joins     │
       │ • Aggregates│
       │ • Filters   │
       └──────┬──────┘
              │
       ┌──────▼──────┐
       │   Menu      │
       │ Interface   │
       │ (REPL)      │
       └─────────────┘
```

---

## Usage Instructions

### Setting Up the Development Environment:

#### Option 1: Using Docker (Recommended)
```bash
# 1. Build the container
docker build -t comp6411-env .

# 2. Run with volume mounting for code persistence
docker run -it --name comp6411 -v "$(pwd):/workspace" comp6411-env

# 3. Use the provided runner scripts
# Windows: ContainerRunner.bat
# Linux/Mac: ./ContainerRunner.sh
```

#### Option 2: Manual Environment Setup
```bash
# Install Erlang
sudo apt-get install erlang  # Ubuntu/Debian
brew install erlang          # macOS

# Install Clojure and Leiningen
curl -O https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod +x lein
sudo mv lein /usr/local/bin/

# Install Python dependencies
pip install socketserver  # Usually included in standard library
```

### Running Individual Components:

#### Assignment 1: Customer Database
```bash
# Terminal 1 - Start server
cd AS1
python server.py

# Terminal 2 - Run client
python client.py

# Expected server output:
# Server started
# 127.0.0.1 wrote: find|john

# Expected client output:
# Customer Management Menu
# 1. Find customer
# 2. Add customer
# ...
```

#### Assignment 2: Sales System
```bash
cd AS2

# Method 1: Using Clojure directly
clojure -M -m menu

# Method 2: Using Leiningen
lein run

# Expected output:
# ***** Sales Menu *****
# _________________________
# 1. Display Customers
# 2. Display Products
# ...
```

#### Project: RPS Tournament
```bash
cd Project

# Compile Erlang modules
erlc game.erl player.erl

# Run tournament
erl -noshell -s game start p1.txt -s init stop

# Sample output:
# ** Rock, Paper Scissors World Championship **
# Starting game log...
# + [1] new game for jill -> sam
# $ (1) jill:paper -> sam:rock = sam lost [25 Credits Left]
```

---

## Testing

### Assignment 1 Test Cases:
```python
# Test data validation
test_cases = [
    ("john", "25", "123 Street", "123-4567"),  # Valid
    ("john123", "25", "123 Street", "123-4567"), # Invalid name
    ("john", "150", "123 Street", "123-4567"),  # Invalid age
    ("john", "25", "123@Street", "123-4567"),   # Invalid address
    ("john", "25", "123 Street", "1234567"),    # Invalid phone
]
```

### Project Test Scenarios:
```erlang
% Test player configurations
test_players = [
    {sam, 26},    % High credits
    {jill, 12},   % Medium credits
    {ahmad, 17},  % Medium credits
    {mia, 20},    % High credits
    {khalifa, 15} % Medium credits
]

% Expected tournament progression:
% 1. Random matchmaking
% 2. Credit deduction on loss
% 3. Disqualification at 0 credits
% 4. Continue until one player remains
```

### Performance Testing:
```bash
# Monitor Erlang processes
erl
> observer:start().  # GUI process monitor

# Monitor system resources during tournament
docker stats comp6411
```

---

## Design Decisions

### 1. Concurrent Architecture (Erlang Project)
- **Choice**: Actor model with lightweight processes
- **Rationale**: Natural fit for tournament simulation where each player operates independently
- **Benefits**: Fault isolation, scalability, no shared state issues

### 2. Functional Programming (Clojure Assignment)
- **Choice**: Immutable data structures and pure functions
- **Rationale**: Sales data processing benefits from referential transparency
- **Benefits**: Thread safety, predictable behavior, easier testing

### 3. TCP/IP Protocol (Python Assignment)
- **Choice**: Custom pipe-separated protocol over TCP
- **Rationale**: Simple, human-readable, easy to debug
- **Benefits**: No external dependencies, cross-platform compatibility

### 4. Containerized Development
- **Choice**: Docker-based environment
- **Rationale**: Consistent development environment across platforms
- **Benefits**: Reproducible builds, easy dependency management

---

## Troubleshooting

### Common Issues and Solutions:

#### 1. Docker Container Won't Start
```bash
# Check Docker service
sudo systemctl status docker  # Linux
docker info                   # All platforms

# Remove conflicting containers
docker rm -f comp6411

# Rebuild with clean cache
docker build --no-cache -t comp6411 .
```

#### 2. Erlang Compilation Errors
```bash
# Ensure correct Erlang version
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'

# Check module dependencies
# game.erl must be compiled before player.erl
erlc game.erl && erlc player.erl
```

#### 3. Clojure Classpath Issues
```bash
# Check Leiningen configuration
lein version

# Run with explicit classpath
clojure -Sdeps '{:deps {}}' -M -m menu
```

#### 4. Python Socket Errors
```python
# Check port availability
netstat -an | grep 9999  # Linux/Mac
netstat -ano | findstr 9999  # Windows

# Try alternative port in server.py
HOST, PORT = "localhost", 9998
```

### Debugging Techniques:

#### Erlang Process Tracing:
```erlang
% Enable tracing for specific processes
dbg:tracer().
dbg:p(all, c).
dbg:tpl(game, handle_game_request, x).

% Monitor message passing
erl -boot start_sasl  # For enhanced logging
```

#### Python Debugging:
```python
# Add debug prints to server.py
import pdb
pdb.set_trace()  # Interactive debugging

# Log all client requests
with open('server.log', 'a') as f:
    f.write(f"Request: {self.data}\n")
```

#### Clojure REPL Debugging:
```clojure
;; Start REPL for interactive debugging
lein repl

;; Load and test functions
(require '[db :as db])
(db/build-customer-table)

;; Use println for debugging
(println "Current state:" customers)
```

---

## Academic Integrity

This repository is intended for educational purposes and as a portfolio piece. Students currently taking COMP 6411 should use this as a reference only and submit their own original work in accordance with Concordia University's Academic Code of Conduct.

## License

Educational Use Only - All rights reserved by the course instructor and Concordia University.

## Contact

For questions about this repository:
- Course: COMP 6411 Programming and Data Structures
- Institution: Concordia University, Montreal
- Semester: Summer 2024

---

*This README was generated as part of the COMP 6411 coursework to demonstrate comprehensive documentation practices for software projects.*
