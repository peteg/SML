/* D3 Tree */
/* Copyright 2013 Peter Cook (@prcweb); Licensed MIT */
/* http://animateddata.co.uk/lab/d3-tree/ */

// Tree configuration
// FIXME encapsulate the dynamic state, shuffle the static stuff down.
// static
var da = 0.5; // Angle delta
var dl = 0.8; // Length delta (factor)
var ar = 0.7; // Randomness
var maxDepth = 10;

// dynamic
var branches = [];

// Tree creation functions

// Return endpoint of branch
function endPt(b) {
  var x = b.x + b.l * Math.sin(b.a);
  var y = b.y - b.l * Math.cos(b.a);
  return {x: x, y: y};
};

function branch(b) {
  var end = endPt(b);

  branches.push(b);

  if (b.d === maxDepth)
    return;

  // Left branch
  var daR = ar * Math.random() - ar * 0.5;
  var newB = {
    i: branches.length,
    x: end.x,
    y: end.y,
    a: b.a - da + daR,
    l: b.l * dl,
    d: b.d + 1,
    parent: b.i
  };
  branch(newB);

  // Right branch
  daR = ar * Math.random() - ar * 0.5;
  newB = {
    i: branches.length,
    x: end.x,
    y: end.y,
    a: b.a + da + daR,
    l: b.l * dl,
    d: b.d + 1,
    parent: b.i
  };
  branch(newB);
};

// D3 functions
function x1(d) {return d.x;}
function y1(d) {return d.y;}
function x2(d) {return endPt(d).x;}
function y2(d) {return endPt(d).y;}

function highlightParents(d) {
  var colour = d3.event.type === 'mouseover' ? 'green' : '#777';
  var depth = d.d;
  for(var i = 0; i <= depth; i++) {
    d3.select('#id-'+parseInt(d.i)).style('stroke', colour);
    d = branches[d.parent];
  }
}

function create() {
  d3.select('#tree')
    .selectAll('line')
    .data(branches)
    .enter()
    .append('line')
    .attr('x1', x1)
    .attr('y1', y1)
    .attr('x2', x2)
    .attr('y2', y2)
    .style('stroke-width', function(d) {return parseInt(maxDepth + 1 - d.d) + 'px';})
    .attr('id', function(d) {return 'id-'+d.i;})
    .on('mouseover', highlightParents)
    .on('mouseout', highlightParents);
}

function update() {
  d3.select('#tree')
    .selectAll('line')
    .data(branches)
    .transition()
    .attr('x1', x1)
    .attr('y1', y1)
    .attr('x2', x2)
    .attr('y2', y2);
}

function regenerate(initialise) {
  var div = document.getElementById("tree_div");
  var h = div.clientHeight;
  var w = div.clientWidth / 2;

  var seed = {i: 0, x: w, y: h, a: 0, l: 130, d:0}; // a = angle, l = length, d = depth

  branches = [];
  branch(seed);
  initialise ? create() : update();
};

// Top level.
window.onload = function() {
  d3.selectAll('#button-ui')
    .on('click', function() { regenerate(false); });

 regenerate(true);
};
