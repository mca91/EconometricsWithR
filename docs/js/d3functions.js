// formatter function for equation text

var  eqtext = function(coefs) {
  if (coefs[0].m < 0) { 
    return( "$\\widehat{Y} = " + d3.round(coefs[0].b,2) + " - " + Math.abs(d3.round(coefs[0].m,2)) + " \\cdot X$" ); 
} else {
    return( "$\\widehat{Y} = " + d3.round(coefs[0].b,2) + " + " + d3.round(coefs[0].m,2) + " \\cdot X$" );
} 
};


var SSR = function(data) {
  var xValuesE = data.map(function(d){return x.invert(d.x);});
  var yValuesE = data.map(function(d){return y.invert(d.y);});
  var lsCoefE = [LeastSquares(xValuesE, yValuesE)];
  var dataE = [xValuesE, yValuesE];
  var yhat = dataE[0].map(function(d) {return lsCoefE[0].b + d * lsCoefE[0].m; } );
  var uhat = yhat.map(function(d,i) {return d - dataE[1][i]; });
  var SSR = d3.sum( uhat.map(function(d) { return d*d; }) );
  return SSR;
}

var R2 = function(data) {
  var yValuesE = data.map(function(d){return y.invert(d.y);});
  var ssr = SSR(data);
  var tss = d3.sum(yValuesE.map(function(d) { return (d - d3.mean(yValuesE))*(d - d3.mean(yValuesE)); }));
  return 1-ssr/tss;
}

var cor = function(data) {
  var xValuesE = data.map(function(d){return x.invert(d.x);});
  var yValuesE = data.map(function(d){return y.invert(d.y);});
  var meanx = d3.mean(xValuesE);
  var meany = d3.mean(yValuesE);
  var sdx = d3.deviation(xValuesE);
  var sdy = d3.deviation(yValuesE);
  var temp = [];
  for (var i=0; i<data.length; i++) {
    temp[i] = 1/(data.length-1)*(xValuesE[i]-meanx)*(yValuesE[i]-meany)/(sdx*sdy);
  }
  var corr = d3.sum(temp);
  return corr;
};


var drawline = function(data){

  var xValues = data.map(function(d){return d.x;});
  var yValues = data.map(function(d){return d.y;});
  var lsCoef = [LeastSquares(xValues, yValues)];

  var xValuesE = data.map(function(d){return x.invert(d.x);});
  var yValuesE = data.map(function(d){return y.invert(d.y);});
  var lsCoefE = [LeastSquares(xValuesE, yValuesE)];

  var lineFunction = d3.svg.line()
    .x(function(d) { return d.x; })
    .y(function(d) { return d.y; });

  svg.append('path')
    .attr("d", lineFunction([{"x": margin, "y": lsCoef[0].m * margin + lsCoef[0].b},{"x": x(20), "y": lsCoef[0].m * x(20) + lsCoef[0].b}]))
    .attr("stroke-width", 1.25)
    .attr("stroke", "darkred")
    .attr('id', 'regline')
    .attr('clip-path', "url(#chart-area)");

svg.append("text")
     .attr("class","subject")
     .attr("x", w-225)
     .attr("y", 40)
     .attr("text-anchor", "left")
     .text("Estimated Model:")
     .style("font-size","10pt");

svg.append("foreignObject")
     .attr("class", "SSR")
         .attr("x", w-225)
         .attr("y", 80)
         .attr("width",100)
         .attr("height",50)
         .attr("text-anchor", "left")
         .text("$SSR = "+ d3.round(SSR(data),2) + "$");

svg.append("foreignObject")
     .attr("class", "R2")
         .attr("x", w-225)
         .attr("y", 95)
         .attr("width",100)
         .attr("height",50)
         .attr("text-anchor", "left")
         .text("$R^2 = " + d3.round(R2(data),2) + "$");

  svg.append("foreignObject")
     .attr("class", "equation")
     .attr("x", w-225)
     .attr("y", 60)
     .attr("width",150)
     .attr("height",50)
     .attr("text-anchor", "left")
     .text(eqtext(lsCoefE));


  svg.append("text")
     .attr("class","subject")
     .attr("x", w-225)
     .attr("y", 135)
     .attr("text-anchor", "left")
     .text("Data:")
     .style("font-size","10pt");


    svg.append("foreignObject")
       .attr("class", "n")
           .attr("x", w-225)
           .attr("y", 150)
           .attr("width",100)
           .attr("height",50)
           .attr("text-anchor", "left")
           .text("$n = " + data.length + "$");

    svg.append("foreignObject")
     .attr("class", "meanx")
         .attr("x", w-225)
         .attr("y", 170)
         .attr("width",100)
         .attr("height",50)
         .attr("text-anchor", "left")
         .text("$\\overline{X} = " + d3.round(d3.mean(xValuesE),2) + "$");

    svg.append("foreignObject")
     .attr("class", "meany")
         .attr("x", w-225)
         .attr("y", 190)
         .attr("width",100)
         .attr("height",50)
         .attr("text-anchor", "left")
         .text("$\\overline{Y} = " + d3.round(d3.mean(yValuesE),2) + "$");

  svg.append("foreignObject")
     .attr("class", "cor draggable")
         .attr("x", w-225)
         .attr("y", 215)
         .attr("width",120)
         .attr("height",50)
         .attr("text-anchor", "left")
         .text("$\\text{Cor}(X,Y) = " + d3.round(cor(data),2) + "$");

  svg.selectAll("text.meany, .meanx, .cor, .n, .equation, .SSR, .subject, .R2")
   .style({
        "font-family":"Century Gothic, CenturyGothic, AppleGothic, sans-serif",
        "font-size": "12px"
   });

   MathJax.Hub.Queue(["Typeset", MathJax.Hub]);

  }



var transitionline = function(data){
  var xValues = data.map(function(d){return d.x;});
  var yValues = data.map(function(d){return d.y;});
  var lsCoef = [LeastSquares(xValues, yValues)];

  var xValuesE = data.map(function(d){return x.invert(d.x);});
  var yValuesE = data.map(function(d){return y.invert(d.y);});
  var lsCoefE = [LeastSquares(xValuesE, yValuesE)];

  var lineFunction = d3.svg.line()
  .x(function(d) { return d.x; })
  .y(function(d) { return d.y; });
  
  d3.select('#regline')
    .transition()
    .duration(500)
    .attr("d", lineFunction([{"x": margin, "y": lsCoef[0].m * margin + lsCoef[0].b},{"x": x(20), "y": lsCoef[0].m * x(20) + lsCoef[0].b}]))

      svg.select("foreignObject.equation")
         .text(eqtext(lsCoefE));

      svg.select("foreignObject.n")
         .text("$n = " + data.length + "$");

      svg.select("foreignObject.SSR")
         .text("$SSR = "+ d3.round(SSR(data),2) + "$");

      svg.select("foreignObject.cor")
         .text("$\\text{Cor}(X,Y) = " + d3.round(cor(data),3) + "$");

      svg.select("foreignObject.R2")
         .text("$R^2 = " + d3.round(R2(data),2) + "$");

      svg.select("foreignObject.meanx")
         .text("$\\overline{X} = " + d3.round(d3.mean(xValuesE),2) + "$");

      svg.select("foreignObject.meany")
         .text("$\\overline{Y} = " + d3.round(d3.mean(yValuesE),2) + "$");


      MathJax.Hub.Queue(["Typeset", MathJax.Hub]);

}

var drawresiduals = function(data){
//get least squares coeffs, great dotted red paths
  var xValues = data.map(function(d){return d.x;});
  var yValues = data.map(function(d){return d.y;});
  var lsCoef = [LeastSquares(xValues, yValues)];

  var lineFunction = d3.svg.line()
    .y(function(d) { return d.y;
    })
    .x(function(d) { return d.x; });


  var resids = data.map(function(d){
    return {"x0": d.x, "y0": d.y, "x1": d.x , "y1": lsCoef[0].m * d.x + lsCoef[0].b}
})

  var halfcircles = function(d){
    var radius = r(200),
      padding = 10,
      radians = Math.PI;

    var dimension = (2 * radius) + (2 * padding),
        points = 50;



    var angle = d3.scale.linear()
        .domain([0, points-1])
        .range([ 0, radians]);

    var fullangle = d3.scale.linear()
        .domain([0, points-1])
        .range([ 0, 2*radians]);

    var line = d3.svg.line.radial()
        .interpolate("basis")
        .tension(0)
        .radius(radius)
        .angle(function(e, i) { 
          if(d.y0-d.y1 < -r(200)) {
            return angle(i) + Math.PI/2; 
          } else if (d.y0 - d.y1 > r(200)){
            return angle(i) + Math.PI*(3/2);
          } else {
            return fullangle(i);
          }
        })


    svg.append("path").datum(d3.range(points))
        .attr("class", "line")
        .attr("d", line)
        .attr("fill", 'none')
        .attr("transform", "translate(" + (d.x0) + ", " + (d.y0) + ")")
        .style("stroke-dasharray", ("1, 1"))
        .style("stroke", function(e){
          if(d.y0-d.y1 > -r(200) && d.y0 - d.y1 < r(200)){
            return "green";

          } else {

            return "red";
          }
        })
        .attr("class", "halfcirc");
    }


  svg.selectAll('path.resline').remove();
  svg.selectAll('path.halfcirc').remove();
  var selection = svg.selectAll('.resline').data(resids)
    
  selection.enter().append('path').transition()
    .attr("d", function(d){
      if(d.y0-d.y1 < -r(200)) {
        return lineFunction([{"x": d.x0, "y": d.y0 + r(200)},{"x": d.x1, "y": d.y1}]); 
      } else if (d.y0 - d.y1 > r(200)){
        return lineFunction([{"x": d.x0, "y": d.y0 - r(200)},{"x": d.x1, "y": d.y1}]);
      } 
    })
    .attr("stroke-width", 1)
    .attr("stroke", "red")
    .attr('class', 'resline')


  selection.exit().remove()


  selection.each(function(d){
    halfcircles(d);
  })
  return resids;
}