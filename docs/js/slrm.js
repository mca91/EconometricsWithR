//Global Variables
var datapoints = [];
var resids = [];

//D3 Set up
var widthslrm = 800,
    heightslrm = 400, 
    marginslrm = 30;

//makes scales
var svgslrm = d3.select("#vizslrm")
            .append("svg")
            .attr("width",widthslrm)
            .attr("height",heightslrm);

var xslrm = d3.scale
          .linear()
          .domain([0,20])
          .range([marginslrm,widthslrm-200]);

var yslrm = d3.scale
          .linear()
          .domain([0,10])
          .range([heightslrm-marginslrm,marginslrm]);

var r = d3.scale
          .linear()
          .domain([0,widthslrm-200])
          .range([0,30]);

var o=d3.scale.linear().domain([10000,100000]).range([.5,1]);
var c=d3.scale.category10().domain(["Africa","America","Asia","Europe","Oceania"]);

//create axis
var xAxis = d3.svg.axis()
  .scale(xslrm)
  .orient("bottom");

var yAxis = d3.svg.axis()
  .scale(yslrm)
  .orient("left");

//draw axis

svgslrm.append("g")
  .attr("class", "axis")
  .attr("transform", "translate(0," + (heightslrm - marginslrm) + ")")
  .call(xAxis)
  .style({
    "fill":"none",
    "stroke":"#000",
    "shape-rendering":"crispEdges",
  });

svgslrm.append("g")
  .attr("class", "axis")
  .attr("transform", "translate(" + marginslrm + ",0)")
  .call(yAxis)
  .style({
    "fill":"none",
    "stroke":"#000",
    "shape-rendering":"crispEdges",
  });;

svgslrm.selectAll("text")
      .style({"font": "12px sans-serif"});

//draw dashed lines
svgslrm.selectAll("horizontal")
   .data(d3.range(0,10,2))
   .enter()
   .append("line")
   .attr("x1",marginslrm)
   .attr("x2",widthslrm-200)
   .attr("y1",yslrm)
   .attr("y2",yslrm)
   .style({
      "stroke":"black",
      "stroke-dasharray":"4 4",
      "stroke-width":"1",
      "stroke-opacity":".5"
   });
  
svgslrm.selectAll("vertical")
   .data(d3.range(0,20,2))
   .enter()
   .append("line")
   .attr("y1",marginslrm)
   .attr("y2",heightslrm-marginslrm)
   .attr("x1",xslrm)
   .attr("x2",xslrm)
   .style({
      "stroke":"black",
      "stroke-dasharray":"4 4",
      "stroke-width":"1",
      "stroke-opacity":".5"
   });

svgslrm.append("text")
     .attr("class","subject")
     .attr("x", widthslrm-150)
     .attr("y", 40)
     .attr("text-anchor", "left")
     .style({"font": "12px sans-serif"

     })
     .text("Estimated Equation:");

var residview = false;

d3.select('#resid_button').on('click', function() {

    if ( residview ) {
        svgslrm.selectAll('path.resline').remove();
        svgslrm.selectAll('path.halfcirc').remove();
        svgslrm.selectAll("circle")
          .style("opacity", 1)
        residview = false;
    } else {
        svgslrm.selectAll("circle")
        .style("opacity", 0)


        residview = true;
        drawresiduals(datapoints);
       
    }        
});

svgslrm.on('dblclick', function() {

   
        svgslrm.selectAll('path.resline').remove();
        svgslrm.selectAll('path.halfcirc').remove();
        svgslrm.selectAll('circle')
           .transition()
           .duration(500)
           .attr("cx", function() { return Math.random(); })
           .remove();

        svgslrm.selectAll('#regline').remove();
        svgslrm.selectAll('text.equation').remove();
        svgslrm.selectAll('text.n').remove();
        svgslrm.selectAll('text.SSR').remove();
        svgslrm.selectAll('text.cor').remove();
        residview = false;
        datapoints = []
        resids = []
        
});  


//click event: draw new circle
svgslrm.on('click', function(){
  if(d3.mouse(this)[0] > (marginslrm + r(200)) && 
      d3.mouse(this)[0] < (widthslrm-200 - r(200)) && 
      d3.mouse(this)[1] > (marginslrm + r(200)) && 
      d3.mouse(this)[1] < (heightslrm-marginslrm - r(200)))
  {
    //push new data point to data array
    datapoints.push({"x": d3.mouse(this)[0], "y": d3.mouse(this)[1], "radius": 100, "fill": "Europe", "opacity": 90000});

    //select each circle and append the data
    var selection = svgslrm.selectAll("circle").data(datapoints)

    //update selection and draw new circle
    selection.enter()
    .append("circle")
    .attr("cx",function(d) {return d.x;})
    .attr("cy",function(d) {return d.y;})
    .attr("r",function(d) {return r(d.radius);})
    .style("fill",function(d) {return "steelblue";})
    .style("opacity",function(d) {
      if(residview){
        return 10;
      } else {
        return o(+d.opacity);
      }
    })

    //exit selection
    selection.exit().remove()

    if(datapoints.length == 2){
      drawline(datapoints);
    } else if(datapoints.length > 2){
      transitionline(datapoints);
      if(residview){
        resids = drawresiduals(datapoints);
      }
    }
  }
})