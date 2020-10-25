# Erlang_Project
<br />
<p align="center">

  <h3 align="center">RPL communications protocol simulator</h3>
  <h5 align="center">Amir sarusi, Yoav levi</h5>

  <p align="center">
    Out final project on 'Concurrent and Distributed Programming' course, Ben Gurion University of the Negev.
    <br />
  </p>
</p>



<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the Project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [Contact](#contact)



<!-- ABOUT THE PROJECT -->
## About The Project
Our project offfers an different interpetation of the R.P.L  communication protocol, the main difference is that in normal R.P.L circumstances, all nodes are static whilst in our simulator, noded move around a 1000X1000 grid. Our project offers some:
* Distributed BFS
* Shortest Path Between Two Vertices (Bellman Ford)
* communication between many different and independent modules
* video of a sample of running the project can be found here: https://youtu.be/teE5jcW7z0M


### Built With
* [Erlang](https://www.erlang.org/) - version 23


<!-- GETTING STARTED -->
## Getting Started
The git offers 2 impelemtations of the design, one done locally (on 4 terminals) and the other in a distributed fashion. 
before getting started, you must choose which one you want to run, and then continue.
open 4 terminals and create 4 4 erlang nodes with the names m_node,g_node,r_node,n_node, once you've done that,
Open folder and terminal for the 'master' node, open a folder and terminal for each one of the submasters.
in each terimnal write:
```sh
git clone hhttps://github.com/yoav130193/Erlang_Project.git
cd Erlang_Project
```

<h3>m_node</h3>
```sh
erl
```
compile 'app_test','script':
```sh
c(app_test).
c(script).
app_test:startRun()
```

<h3>r_node or g_node or n_node</h3>
```sh
erl
```
compile ,'script':
```sh
c(script).
script:compileAll().
wrapper:start_link.
```

<!-- USAGE EXAMPLES -->
## Usage
<br />

* Choose whether you want the R.P.L server to work in storing or non-stroring mode, once you have chosen either of them, press the "start" button on the GUI.
* start spawning Nodes/roots.
* choose a source node for sending a message
* choose Destination nodes, however many as you want.
* write in the text box the message to send.
* press send message once ready, if the message can be sent, a brief route of the message will be drawn on the GUI.
<br />
<br />

![GitHub Logo](/1.jpg)
Format: ![Alt Text](url)


<!-- CONTACT -->
## Contact

Amir sarusi - sarusiam [at ] post.bgu.ac.il

Yoav levi - leviy [at ] post.bgu.ac.il


