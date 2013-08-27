/**
*
*
*
*
*
*
*
*
*/

var FLG = false;

Reveal.addEventListener( 'viz-charts', function() {
	if (!FLG) {
		createCharts();
		FLG = true;
	};
});


function createCharts() {

	var line = Chart.line(),
		height = 400,
		format = d3.time.format('%H'),
		xLabel = 'Time (hour of day)',
		yLabel = '#Tweets (millions)',
		xTicks = 6,
		yTicks = 5;

	// Load the CSV data:
	d3.csv( 'data/1.csv', function( data ) {
		
		/*data.forEach( function(d) { 
			d.time = format.parse(d.time);
		} );*/

		data = data.map( function(d) {
			return {
				'x': +d.time,
				'y': +d.tweets, // force type conversion
				'label': 'Tweets'
			};
		});


		// LINE CHART //

		// Append a new figure to the DOM:
		figure = d3.select( '.chart1' )
			.append( 'figure' )
				.attr('class', 'chart');

		// Get the figure width:
		width = parseInt( figure.style( 'width' ), 10 );

		// Update the chart generator settings:
		line.width( width )
			.height( height )
			//.xScale( d3.time.scale() )
			.xLabel( xLabel )
			.xTicks( xTicks )
			.yTicks( yTicks )
			.yLabel( yLabel );		

		// Bind the data and generate a new chart:
		figure.datum( data )
			.call( line );	

	});	





	var multiline = Chart.multiline(),
		height = 400,
		format = d3.time.format('%H'),
		xLabel = 'Time (hour of day)',
		yLabel = '#Tweets (millions)',
		xTicks = 6,
		yTicks = 5,
		datasets,
		headings = ['sf', 'nyc', 'london', 'moscow', 'delhi', 'manilla'],
		legend = ['SF', 'NYC', 'LDN', 'Moscow', 'Delhi', 'Manilla'];

	// Load the CSV data:
	d3.csv( 'data/2.csv', function( data ) {
		
		datasets = headings.map( function(heading) {
			return data.map( function(d) {
				return {
					'x': +d['x_'+heading],
					'y': +d['y_'+heading], // force type conversion
					'label': 'Tweets: ' + heading
				};
			});
		});


		// LINE CHART //

		// Append a new figure to the DOM:
		figure = d3.select( '.chart2' )
			.append( 'figure' )
				.attr('class', 'chart');

		// Get the figure width:
		width = parseInt( figure.style( 'width' ), 10 );

		// Update the chart generator settings:
		multiline.width( width )
			.height( height )
			.xLabel( xLabel )
			.xTicks( xTicks )
			.yTicks( yTicks )
			.yLabel( yLabel );		

		// Bind the data and generate a new chart:
		figure.datum( datasets )
			.call( multiline );	

	});	







	var interactive = Chart.interactive(),
		height = 400,
		format = d3.time.format('%H'),
		xLabel = 'Time (hour of day)',
		yLabel = '#Tweets (millions)',
		xTicks = 6,
		yTicks = 5,
		datasets,
		headings = ['sf', 'nyc', 'london', 'moscow', 'delhi', 'manilla'],
		legend = ['SF', 'NYC', 'London', 'Moscow', 'Delhi', 'Manilla'];

	// Load the CSV data:
	d3.csv( 'data/2.csv', function( data ) {
		
		datasets = headings.map( function(heading) {
			return data.map( function(d) {
				return {
					'x': +d['x_'+heading],
					'y': +d['y_'+heading], // force type conversion
					'label': heading
				};
			});
		});


		// LINE CHART //

		// Append a new figure to the DOM:
		figure = d3.select( '.chart3' )
			.append( 'figure' )
				.attr('class', 'chart');

		// Get the figure width:
		width = parseInt( figure.style( 'width' ), 10 );

		// Update the chart generator settings:
		interactive.legend( legend )
			.width( width )
			.height( height )
			.xLabel( xLabel )
			.xTicks( xTicks )
			.yTicks( yTicks )
			.yLabel( yLabel );		

		// Bind the data and generate a new chart:
		figure.datum( datasets )
			.call( interactive );	

	});	

}; // end FUNCTION createCharts()


