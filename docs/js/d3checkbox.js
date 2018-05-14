function d3CheckBox () {

    var size = 20,
        x = 0,
        y = 0,
        rx = 0,
        ry = 0,
        boxStrokeColor = "black",
        markStrokeColor = boxStrokeColor,
        boxStrokeWidth = 3,
        markStrokeWidth = 3,
        checked = false,
        defaultText = "text",
        clickEvent;

    function checkBox (selection) {

        var g = selection.append("g"),
            box = g.append("rect")
            .attr("width", size)
            .attr("height", size)
            .attr("x", x)
            .attr("y", y)
            .attr("rx", rx)
            .attr("ry", ry)
            .style({
                "fill-opacity": 0,
                "stroke-width": boxStrokeWidth,
                "stroke": boxStrokeColor
            });
            g.append("foreignObject")
                   .attr("x", x+1.5*size)
                   .attr("y", y+y*(1/3) )
                   .attr("text-anchor", "left")
                   .style("font-size",size*2/3)
                   .text(defaultText);

        //Data to represent the check mark
        var coordinates = [
            {x: x + (size / 8), y: y + (size / 3)},
            {x: x + (size / 2.2), y: (y + size) - (size / 4)},
            {x: (x + size) - (size / 8), y: (y + (size / 10))}
        ];

        var line = d3.svg.line()
                .x(function(d){ return d.x; })
                .y(function(d){ return d.y; })
                .interpolate("basic");

        var mark = g.append("path")
            .attr("d", line(coordinates))
            .style({
                "stroke-width" : markStrokeWidth,
                "stroke" : markStrokeColor,
                "fill" : "none",
                "opacity": (checked)? 1 : 0
            });

        g.on("click", function () {
            checked = !checked;
            mark.style("opacity", (checked)? 1 : 0);

            if(clickEvent)
                clickEvent();

            d3.event.stopPropagation();
        });

    }

    checkBox.text = function (val) {
        defaultText = val;
        return checkBox;
    }

    checkBox.size = function (val) {
        size = val;
        return checkBox;
    }

    checkBox.x = function (val) {
        x = val;
        return checkBox;
    }

    checkBox.y = function (val) {
        y = val;
        return checkBox;
    }

    checkBox.rx = function (val) {
        rx = val;
        return checkBox;
    }

    checkBox.ry = function (val) {
        ry = val;
        return checkBox;
    }

    checkBox.markStrokeWidth = function (val) {
        markStrokeWidth = val;
        return checkBox;
    }

    checkBox.boxStrokeColor = function (val) {
        boxStrokeColor = val;
        markStrokeColor = val;
        return checkBox;
    }

    checkBox.boxStrokeWidth = function (val) {
        boxStrokeWidth = val;
        return checkBox;
    }

    checkBox.checked = function (val) {

        if(val === undefined) {
            return checked;
        } else {
            checked = val;
            return checkBox;
        }
    }

    checkBox.clickEvent = function (val) {
        clickEvent = val;
        return checkBox;
    }

    return checkBox;
}