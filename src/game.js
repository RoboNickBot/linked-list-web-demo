
//document.getElementById("button").onclick = startGame;
//
//Start immediately instead
startGame();

function startGame() {
  var c = document.getElementById("game");
  var ball = { color:"black", size:50, phys:{pos:{x:250, y:250}, vel:{x:5, y:8}}}
  var gamestate = {context:c.getContext("2d"), ball:ball};

  c.addEventListener("mousedown", function(event) {colorShift(event, gamestate);}, false);

  var id = window.setInterval(function() {upd8(gamestate);}, 36);

}

function colorShift(event, state)
{
  var c = document.getElementById("game");
  var pos = state.ball.phys.pos;

  var e = this;
  var x = 0;
  var y = 0;

  cOffset = $(c).offset();
  x = event.clientX
    + document.body.scrollLeft 
    + document.documentElement.scrollLeft 
    - Math.floor(cOffset.left);
  y = event.clientY 
    + document.body.scrollTop 
    + document.documentElement.scrollTop 
    - Math.floor(cOffset.top) + 1;

  //var x = event.clientX + document.body.offsetLeft + document.documentElement.offsetLeft;
  //var y = event.clientY + document.body.offsetTop + document.documentElement.offsetTop;

  if(x >= (pos.x - state.ball.size) && x <= (pos.x + state.ball.size)
      && y >= (pos.y - state.ball.size) && y <= (pos.y + state.ball.size))
  {
    if(state.ball.color == "blue")
    {
      state.ball.color = "red";
    }
    else
    {
      state.ball.color = "blue";
    }
  }
  else
  {
    state.ball.color = "black";
  }
}

function upd8(state) {
  var pos = state.ball.phys.pos;
  var vel = state.ball.phys.vel;
  var size = state.ball.size;

  pos.x += vel.x;
  pos.y += vel.y;

  if (pos.x < size || pos.x > (600 - size)) {
    pos.x -= vel.x;
    vel.x *= -1;
  }
  if (pos.y < size || pos.y > (500 - size)) {
    pos.y -= vel.y;
    vel.y *= -1;
  }

  drawBall(state, 1);
}

function drawBall(state, aVal) {
  var pos = state.ball.phys.pos;
  state.context.clearRect(0,0,600,500);
  state.context.beginPath();
  state.context.arc(pos.x, pos.y, state.ball.size, 0, 2*Math.PI);
  state.context.fillStyle = state.ball.color;
  state.context.fill();
}
