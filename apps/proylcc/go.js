// Reference to object provided by pengines.js library which interfaces with Pengines server (Prolog-engine)
// by making query requests and receiving answers.
var pengine;
// Bidimensional array representing board configuration.
var gridData;
// Bidimensional array with board cell elements (HTML elements).
var cellElems;
// States if it's black player turn.
var turnBlack = false;
var bodyElem;
var latestStone;

var passedTurn = false;



/**
* Initialization function. Requests to server, through pengines.js library, 
* the creation of a Pengine instance, which will run Prolog code server-side.
*/

function init() {
    document.getElementById("passBtn").addEventListener('click', () => passedTurn? finDelJuego(): passTurn() );
    bodyElem = document.getElementsByTagName('body')[0];
    createBoard();
    // Creación de un conector (interface) para comunicarse con el servidor de Prolog.
    pengine = new Pengine({
        server: "http://localhost:3030/pengine",
        application: "proylcc",
        oncreate: handleCreate,
        onsuccess: handleSuccess,
        onfailure: handleFailure,
        destroy: false
    });
}

/**
 * Create grid cells elements
 */

function createBoard() {
    const dimension = 19;
    const boardCellsElem = document.getElementById("boardCells");
    for (let row = 0; row < dimension - 1; row++) {
        for (let col = 0; col < dimension - 1; col++) {
            var cellElem = document.createElement("div");
            cellElem.className = "boardCell";
            boardCellsElem.appendChild(cellElem);
        }
    }
    const gridCellsElem = document.getElementById("gridCells");
    cellElems = [];
    for (let row = 0; row < dimension; row++) {
        cellElems[row] = [];
        for (let col = 0; col < dimension; col++) {
            var cellElem = document.createElement("div");
            cellElem.className = "gridCell";
            cellElem.addEventListener('click', () => handleClick(row, col));
            gridCellsElem.appendChild(cellElem);
            cellElems[row][col] = cellElem;
        }
    }
}

/**
 * Callback for Pengine server creation
 */

function handleCreate() {
    pengine.ask('emptyBoard(Board)');
}

/**
 * Callback for successful response received from Pengines server.
 */

function handleSuccess(response) {

    gridData = response.data[0].Board;

    //Get data de fin del juego
    var black = response.data[0].Negro;
    var white = response.data[0].Blanco;

    if(black != undefined && white != undefined)
        mostrarYLimpiar(black,white);
    else{

        for (let row = 0; row < gridData.length; row++)
            for (let col = 0; col < gridData[row].length; col++) {
                cellElems[row][col].className = "gridCell" +
                    (gridData[row][col] === "w" ? " stoneWhite" : gridData[row][col] === "b" ? " stoneBlack" : "") +
                    (latestStone && row === latestStone[0] && col === latestStone[1] ? " latest" : "");
            }
        
        switchTurn();
    }
}

/**
 * Called when the pengine fails to find a solution.
 */

function handleFailure() {
    alert("Invalid move!");
}

/**
 * Handler for color click. Ask query to Pengines server.
 */

function handleClick(row, col) {
    const s = "goMove(" + Pengine.stringify(gridData) + "," + Pengine.stringify(turnBlack ? "b" : "w") + "," + "[" + row + "," + col + "]" + ",Board)";
    pengine.ask(s);
    latestStone = [row, col];
    passedTurn = false;
}


function passTurn(){
        passedTurn = true; 
        switchTurn();  
}

function finDelJuego(){
    
    const s = "finDelJuego("+Pengine.stringify(gridData)+", Negro , Blanco)";

    pengine.ask(s);

}

function mostrarYLimpiar(puntosNegro,puntosBlanco){

    var ganador =(puntosNegro==puntosBlanco)? ": EMPATE": (puntosNegro>puntosBlanco)? ": NEGRO" : ": BLANCO";

    alert("GANADOR "+ganador+"\nNegro = "+puntosNegro+"\nBlanco = "+puntosBlanco);
    
    //Limpio el tablero y comienza un juego nuevo
    turnBlack = false;
    passedTurn = false;
    pengine.ask('emptyBoard(Board)');
}

function switchTurn() {
    turnBlack = !turnBlack;
    bodyElem.className = turnBlack ? "turnBlack" : "turnWhite";
}


/**
* Call init function after window loaded to ensure all HTML was created before
* accessing and manipulating it.
*/

window.onload = init;