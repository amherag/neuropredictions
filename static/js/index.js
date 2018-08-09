$(document).ready(()=>{
  let x, y, z, opens, highs, lows, closes;
  
  x = [];
  y = [];
  z = [];

  opens = [];
  highs = [];
  lows = [];
  closes = [];

  isAlertZone = (obj) => {
    var open = obj.open
    var close = obj.close

    var openIndex = 0
    var closeIndex = 0
    
    var curr = obj.heat.y[0];
    var diff = Math.abs (open - curr);
    
    for (var val = 0; val < obj.heat.y.length; val++) {
      var newdiff = Math.abs (open - obj.heat.y[val]);
      if (newdiff < diff) {
        diff = newdiff;
        curr = obj.heat.y[val];
        openIndex = val
      }
    }
    
    curr = obj.heat.y[0];
    diff = Math.abs (close - curr);
    
    for (var val = 0; val < obj.heat.y.length; val++) {
      var newdiff = Math.abs (close - obj.heat.y[val]);
      if (newdiff < diff) {
        diff = newdiff;
        curr = obj.heat.y[val];
        closeIndex = val
      }
    }
    // return true
    if (obj.heat.z[openIndex] > obj.heat.z[closeIndex] * 2) {
      return true
    } else {
      return false
    }
  }

  timeConvert = (time) => {
    let date =  new Date(parseInt(time) / 1000);
    let sec = date.getSeconds();
    let minutes = date.getMinutes();
    let hours = date.getHours();
    if ( parseInt(sec) < 10) { sec = "0" + sec; }
    if ( parseInt(minutes) < 10) { minutes = "0" + minutes; }
    if ( parseInt(hours) < 10) { hours = "0" + hours; }

    return date.getFullYear() + "-" + (date.getMonth() + 1) + "-" + (date.getDate()) + " " + hours + ":" + minutes + ":" + sec;
  };
  
  valuesLayout = (values) => {
    return Object.keys(values[0]).map((c) => values.map((r) => r[c]));
  };

  alertzonesData = (res) => {
    var x = []
    var y = []
    $.each(res, (_, item) => {
      if (isAlertZone(item)) {
        x.push(timeConvert(item.time))
        y.push(item.close)
      }
      
    })
    
    return {
      x: x,
      y: y,
      xaxis: 'x',
      yaxis: 'y',
      type: 'scatter',
      mode: 'markers',
      marker: {
        size: 20,
        color: 'rgba(255,255,255,0.5)',
      },
      showscale: false,
      showlegend: false,
    };
  };
  
  heatmapData = (res) => {
    $.each(res, (i, item) => {
      // z.push(Object.values(sortHeatmapKeys(res[i].heat)))
      z.push(res[i].heat.z)
    });
    
    // y = Object.keys(sortHeatmapKeys(res[0].heat));
    y = res[0].heat.y
    
    $.each(y, (i, item) => {
      y[i] = parseFloat(y[i])
    });
    
    z = valuesLayout(z);
    return {
      z: z,
      x: x,
      y: y,
      xaxis: 'x',
      yaxis: 'y',
      type: 'heatmap',
      // colorscale: 'Bluered',
      colorscale: 'YIOrRd',
      showscale: false,
      colorbar: {
        xpad: 30
      }
    };
  };

  candlechartData = (res) => {
    /* $.each(res, (i, item) => {
     *   y.push(parseFloat(res[i].rate));
     * });*/
    
    $.each(res, (i, item) => {
      opens.push(res[i].open);
    });

    $.each(res, (i, item) => {
      highs.push(res[i].high);
    });

    $.each(res, (i, item) => {
      lows.push(res[i].low);
    });

    $.each(res, (i, item) => {
      closes.push(res[i].close);
    });
    
    return {
      x: x,
      open: opens,
      high: highs,
      low: lows,
      close: closes,

      // line: {color: 'rgba(31,119,180,1)'},
      increasing: {line: {color: 'rgba(255,255,255,1)'}},
      decreasing: {line: {color: '#000'}},

      xaxis: 'x',
      yaxis: 'y',
      type: 'candlestick',
      showlegend: false,
    };
  };

  fetchDataTest = (url, element, title) => {
    $.ajax({
      url: url,
      method: "GET",
      dataType: "json",
      beforeSend: () => {
        $('#loading').show()
      },
      complete: () => {
        $('#loading').hide()
      },
      success: (res) => {
        opens = [];
        highs = [];
        lows = [];
        closes = [];
        
        x = [];
        y = [];
        z = [];
        

        $.each(res, (_, item) => {
          x.push(timeConvert(item.time));
        });

        let layout = {
          title: title,
          height: 600,
          titlefont : {
            size: 18,
          },
          font: {
            family: "Arial",
            size: 14
          },
          xaxis: {
            name: 'Dates',
            titlefont: {
              size: 18
            },
            linecolor: 'white',
            linewidth: 0,
            mirror: true,
            showticklabels: true,
            // tickangle: 30,
            rangeslider: {
              visible: false,
            },
            tickformat: '%y/%m/%d %I:%M:%S',
            // overlaying: 'x2',
          },
          yaxis: {
            side: "right",
            linecolor: 'white',
            linewidth: 0,
            mirror: true,
            // overlaying: 'y2',
          },
          /* xaxis2: {
           *   overlaying: 'x',
           * }, */
          /* yaxis2: {
           *   title: "",
           *   titlefont: {
           *     size: 18
           *   },
           *   // overlaying: 'y2',
           *   side: 'left'
           * }, */
          autosize: true
        };

        let data = [];

        data.push(heatmapData(res));
        data.push(candlechartData(res));
        data.push(alertzonesData(res));

        Plotly.newPlot(element, data, layout, {displayModeBar: false});
      },
      error: (req, status, err) => {
        console.log(req);
        console.log(status);
        // alert(err);
      }
    });
  };
});


var chartsLayout = []

function deleteLayout (layoutName) {
  var layouts = null
  if (localStorage.getItem('layouts') !== null) {
    layouts = JSON.parse(localStorage.getItem('layouts'))
  }
  
  var newLayouts = []
  if (layouts !== null) {
    layouts.forEach((layout) => {
      console.log("name", layoutName, layout.name)
      if (layout.name != layoutName) {
        newLayouts.push(layout)
      }
    })
    console.log("newLayouts", newLayouts)
    localStorage.setItem('layouts', JSON.stringify(newLayouts))
    loadLayoutOptions()
  }
  
  // localStorage.removeItem('layout')
}

function clearLayout () {
  document.getElementById('charts').innerHTML = ""
  chartsLayout = []
}

function deleteAllLayouts () {
  if (confirm('Are you sure you want to clear all your layouts?')) {
    localStorage.clear()
    loadLayoutOptions()
  }
}

function refreshLayout () {
  document.getElementById('charts').innerHTML = ""

  var key = document.getElementById('key').value
  
  chartsLayout.forEach((chart) => {
    var pair = chart.pair
    var tf = chart.timeframe
    var params = key + "/" + pair +"/" + tf
    var id = params + Math.random()

    var row = document.createElement('div')
    row.classList.add('row')
    var ch = document.createElement('div')
    ch.id = id
    ch.classList.add('col-lg-12')

    row.appendChild(ch)
    document.getElementById('charts').appendChild(row)

    
    
    fetchDataTest("/heatmap/"+ params, id, pair + " " + tf)
  })
}

function loadLayout (layoutName) {
  document.getElementById('charts').innerHTML = ""
  var key = document.getElementById('key').value

  var layouts = JSON.parse(localStorage.getItem('layouts'));
  
  layouts.forEach((layout) => {
    if (layout.name == layoutName) {
      var charts = JSON.parse(layout.layout)
      chartsLayout = charts

      charts.forEach((chart) => {
        var pair = chart.pair
        var tf = chart.timeframe
        var params = key + "/" + pair +"/" + tf
        var id = params + Math.random()

        var row = document.createElement('div')
        row.classList.add('row')
        var ch = document.createElement('div')
        ch.id = id
        ch.classList.add('col-lg-12')

        row.appendChild(ch)
        document.getElementById('charts').appendChild(row)

        
        
        fetchDataTest("/heatmap/"+ params, id, pair + " " + tf)
      })
    }
  })

  layouts.selectedIndex = "0";
}

function saveLayout() {
  var layout = {name: $('#layoutName').val(),
                layout: JSON.stringify(chartsLayout)}
  if (localStorage.getItem('layouts') === null) {
    localStorage.setItem('layouts', JSON.stringify([layout]));
    $('#layoutName').val('')
  } else {
    var layouts = JSON.parse(localStorage.getItem('layouts'))

    var alreadyExists = false
    $.each(layouts, (i, lay) => {
      if (lay.name == layout.name) {
        alreadyExists = true
        layouts[i] = layout
      }
    })
    if (!alreadyExists) {
      layouts.push(layout)
    }
    

    localStorage.setItem('layouts', JSON.stringify(layouts));
    $('#layoutName').val('')
  }

  loadLayoutOptions()
}

$(function() {
  var layouts = document.getElementById("layouts")
  var deleteLayouts = document.getElementById("deleteLayouts")
  layouts.onchange = function() {
    loadLayout(layouts.value)
    layouts.selectedIndex = "0";
  }
  deleteLayouts.onchange = function() {
    deleteLayout(deleteLayouts.value)
    deleteLayouts.selectedIndex = "0";
  }
})

function exportLayouts () {
  // prompt("Copy and store somewhere this code:", localStorage.getItem('layouts'))
  $('#exportLayoutsContent').val(localStorage.getItem('layouts'))
}

function importLayouts () {
  var layouts = $('#importLayoutsContent').val()
  localStorage.setItem('layouts', layouts)
  loadLayoutOptions()
}

function loadLayoutOptions () {
  $('#layouts').find('option').remove()
  $('#deleteLayouts').find('option').remove()
  
  $('#layouts').append($('<option>', {
    value: '...',
    text: 'Choose a layout to load'
  }))
  $('#deleteLayouts').append($('<option>', {
    value: '...',
    text: 'Choose a layout to delete'
  }))
  
  if (localStorage.getItem('layouts') !== null) {
    var layouts = JSON.parse(localStorage.getItem('layouts'))

    layouts.sort(function(a, b) {
      var keyA = a.name,
          keyB = b.name;
      
      if (keyA < keyB) return -1;
      if (keyA > keyB) return 1;
      
      return 0;
    })
    
    layouts.forEach((layout) => {
      $('#layouts').append($('<option>', {
        value: layout.name,
        text: layout.name
      }));
      $('#deleteLayouts').append($('<option>', {
        value: layout.name,
        text: layout.name
      }));
    })
  }
}

$(function() {
  // localStorage.clear()
  loadLayoutOptions()
  $('#loading').hide()
})
