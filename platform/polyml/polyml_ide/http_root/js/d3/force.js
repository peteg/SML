// http://bl.ocks.org/mbostock/95aa92e2f4e8345aaa55a4a94d41ce37
// https://jsfiddle.net/t4vzg650/6/
// FIXME consider: sticky force layout: https://bl.ocks.org/mbostock/3750558
"use strict";

function dostuff() {
  // FIXME extract from containing div, update dynamically, tell the center force about it.
  var width = 500;
  var height = 500;

  function click(d) {
    console.log("click: " + d.id);
  };

  function forceSimulation(links, nodes) {
    var simulation = d3.forceSimulation()
        .force("link", d3.forceLink().id(function(d) { return d.id }))
        .force("collide",d3.forceCollide( function(d){return d.r + 8 }).iterations(16) )
        .force("charge", d3.forceManyBody())
        .force("center", d3.forceCenter(width / 2, height / 2))
        .force("y", d3.forceY(0))
        .force("x", d3.forceX(0));

    var svg = d3.select("#force");

    var link_svg = svg
        .selectAll(".line")
        .data(links);

    var link_enter = link_svg
        .enter()
        .append("line")
        .attr("class", "link");

    link_svg
      .exit().remove();

    link_svg = link_enter.merge(link_svg);

    var node_svg = svg
        .selectAll(".node")
        .data(nodes);

    var node_enter = node_svg
        .enter()
        .append("g")
        .attr("class", "node")
        .call(d3.drag()
              .on("start", dragstarted)
              .on("drag", dragged)
              .on("end", dragended));

    node_enter
      .append("circle")
      .attr("r", function(d){ return 2 * d.id.length; })
      .on("click", click)

    node_enter
      .append("text")
      .attr("dy", 3)
      .attr("x", function(d) { return d.children ? -8 : 8; })
      .style("text-anchor", function(d) { return d.children ? "end" : "start"; })
      .text(function(d) { return d.id; });

    node_svg
      .exit().remove();

    node_svg = node_enter.merge(node_svg);

    var ticked = function() {
      link_svg
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });

      node_svg
        .attr("transform", function(d) { return "translate(" + d.x + ", " + d.y + ")"; });
    }

    simulation
      .nodes(nodes)
      .on("tick", ticked);

    simulation
      .force("link")
      .links(links);

    function dragstarted(d) {
      if(!d3.event.active) {
        simulation.alphaTarget(0.3).restart();
      };
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(d) {
      d.fx = d3.event.x;
      d.fy = d3.event.y;
    }

    function dragended(d) {
      if(!d3.event.active) {
        simulation.alphaTarget(0);
      };
      d.fx = null;
      d.fy = null;
    };
  };

  // ----------------------------------------
  // Top level

  // FIXME spaghetti monster
  var handler = (function() {
    var handle_user_data;
    var promise = new Promise(function(resolve, reject) {
      handle_user_data = function(json) {
        console.log('handle_user_data: ' + json);
        resolve(json);
      };
    });

    return {
      data: promise,
      handle_user_data: handle_user_data
    };
  })();

  var polyml = new PolyML(handler.handle_user_data);
  polyml.request("data");

  handler.data.then(json => forceSimulation(json.payload.rels, json.payload.nodes));
};
