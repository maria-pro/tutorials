library(dash)
library(dashCoreComponents)

# Create a Dash app
app <- dash_app()

# Set the layout of the app
app %>% set_layout(
  h1('Hello Dash'),
  div("Dash: A web application framework for your data."),
  dccGraph(
    figure = list(
      data = list(
        list(
          x = list(1, 2, 3),
          y = list(4, 1, 2),
          type = 'bar',
          name = 'SF'
        ),
        list(
          x = list(1, 2, 3),
          y = list(2, 4, 5),
          type = 'bar',
          name = 'Montr\U{00E9}al'
        )
      ),
      layout = list(title = 'Dash Data Visualization')
    )
  )
)

# Run the app
app %>% run_app()
