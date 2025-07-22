library(shiny)
library(bslib)
library(tidyverse)
library(cowplot)

# Define UI ----
ui <- page_sidebar(
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the value of N
    sliderInput(
      inputId = "N",
      label = "Sample size",
      min = 0,
      max = 400,
      value = 10
    ),
    # Input: Slider for mean
    sliderInput(
      inputId = "mu",
      label = "Mean for Beta prior",
      min = 0.01,
      max = 0.99,
      value = 0.5
    ),
    # Input: Slider for 'scale' parameter kappa = alpha + beta
    sliderInput(
      inputId = "kappa",
      label = "'Precision' for Beta prior",
      min = 0.01,
      max = 100,
      value = 4
    ),
    # Widget for selecting which of prior/posterior/likelihood to plot
    checkboxGroupInput(
      inputId = "curve",
      "Select to plot",
      choices = list("Prior" = "prior", "Likelihood" = "likelihood", "Posterior" = "posterior"),
      selected = NULL
    )
  ),
  # Output: Plots
  plotOutput(outputId = "plots")
)

# Define server logic ----
server <- function(input, output) {
  output$plots <- renderPlot({
    # Maximum number of sampled points
    Nmax <- 400
    
    # Maximum sample
    set.seed(1234)
    ymax <- rbinom(Nmax, 1, 0.71)  # proportion of water on Earth ~0.71
    
    # Subset sample of prescribed size
    y <- ymax[1:input$N]
    y_star <- ifelse(input$N == 0, 0, sum(y))  # sufficient statistic
    
    # map mean and precision to alpha and beta
    alpha <- input$mu * input$kappa
    beta <- (1 - input$mu) * input$kappa
    
    # prepare data for plots
    x_seq <- seq(0, 1, length.out = 201)
    y_prior <- sapply(x_seq, function(x) {dbeta(x, alpha, beta)})
    if (input$N == 0) {
      y_lklhd <- rep(1, length(x_seq))
    } else {
      y_lklhd <- sapply(x_seq, function(x) {prod(dbinom(y,1,x))})
    }
    y_post <- sapply(x_seq, function(x) {dbeta(x, alpha + y_star, beta + input$N - y_star)})
    
    # make empty plot
    prior_post_null_plot <- tibble(x = x_seq, prior = y_prior, posterior = y_post) %>%
      pivot_longer(-x, names_to = "distribution", values_to = "density") %>%
      filter(distribution == "prior") %>%
      ggplot(aes(x = x, y = density, alpha = distribution)) +
      geom_line(linewidth = 2, colour = "white") +
      coord_cartesian(xlim = c(0,1), ylim = c(0,10)) +
      theme_classic() +
      theme(
        aspect.ratio = 1,
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.position = "none",
      ) +
      scale_alpha_manual(values = c("prior" = 0.25, "posterior" = 1)) +
      labs(x = expression(theta), y = "probability density") +
      scale_y_continuous(breaks = seq(0,10,length.out = 6)) +
      scale_x_continuous(breaks = seq(0,1,length.out = 6))

    # make prior-only plot
    prior_plot <- tibble(x = x_seq, prior = y_prior, posterior = y_post) %>%
      pivot_longer(-x, names_to = "distribution", values_to = "density") %>%
      filter(distribution == "prior") %>%
      ggplot(aes(x = x, y = density, alpha = distribution)) +
      geom_line(linewidth = 2, colour = "darkred") +
      coord_cartesian(xlim = c(0,1), ylim = c(0,10)) +
      theme_classic() +
      theme(
        aspect.ratio = 1,
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "inside",
        legend.position.inside = c(0.25, 0.85)
      ) +
      scale_alpha_manual(values = c("prior" = 0.25, "posterior" = 1)) +
      labs(x = expression(theta), y = "probability density") +
      scale_y_continuous(breaks = seq(0,10,length.out = 6)) +
      scale_x_continuous(breaks = seq(0,1,length.out = 6))
    
    # make posterior-only plot
    post_plot <- tibble(x = x_seq, prior = y_prior, posterior = y_post) %>%
      pivot_longer(-x, names_to = "distribution", values_to = "density") %>%
      filter(distribution == "posterior") %>%
      ggplot(aes(x = x, y = density, alpha = distribution)) +
      geom_line(linewidth = 2, colour = "darkred") +
      coord_cartesian(xlim = c(0,1), ylim = c(0,10)) +
      theme_classic() +
      theme(
        aspect.ratio = 1,
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "inside",
        legend.position.inside = c(0.25, 0.85)
      ) +
      scale_alpha_manual(values = c("prior" = 0.25, "posterior" = 1)) +
      labs(x = expression(theta), y = "probability density") +
      scale_y_continuous(breaks = seq(0,10,length.out = 6)) +
      scale_x_continuous(breaks = seq(0,1,length.out = 6))
    
    # make prior-posterior plot
    prior_post_plot <- tibble(x = x_seq, prior = y_prior, posterior = y_post) %>%
      pivot_longer(-x, names_to = "distribution", values_to = "density") %>%
      ggplot(aes(x = x, y = density, alpha = distribution)) +
      geom_line(linewidth = 2, colour = "darkred") +
      coord_cartesian(xlim = c(0,1), ylim = c(0,10)) +
      theme_classic() +
      theme(
        aspect.ratio = 1,
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "inside",
        legend.position.inside = c(0.25, 0.85)
      ) +
      scale_alpha_manual(values = c("prior" = 0.25, "posterior" = 1)) +
      labs(x = expression(theta), y = "probability density") +
      scale_y_continuous(breaks = seq(0,10,length.out = 6)) +
      scale_x_continuous(breaks = seq(0,1,length.out = 6))
    
    # make empty likelihood plot
    lklhd_null_plot <- tibble(x = x_seq, y = y_lklhd/max(y_lklhd)) %>%
      ggplot(aes(x=x, y=y)) +
      geom_line(linewidth = 2, colour = "white", alpha = 0.7) +
      coord_cartesian(xlim = c(0,1), ylim = c(0,1.5)) +
      theme_classic() +
      labs(x = expression(theta), y = "likelihood") +
      theme(
        aspect.ratio = 1,
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15)
      ) +
      scale_x_continuous(breaks = seq(0,1,length.out = 6))
      
    # make likelihood plot  
    lklhd_plot <- tibble(x = x_seq, y = y_lklhd/max(y_lklhd)) %>%
      ggplot(aes(x=x, y=y)) +
      geom_line(linewidth = 2, colour = "navyblue", alpha = 0.7) +
      coord_cartesian(xlim = c(0,1), ylim = c(0,1.5)) +
      theme_classic() +
      labs(x = expression(theta), y = "likelihood") +
      theme(
        aspect.ratio = 1,
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15)
      ) +
      scale_x_continuous(breaks = seq(0,1,length.out = 6)) +
      annotate(
        "text", x = 0.4, y = 1.4, size = 7,
        label = str_c(y_star, " of ", input$N, " samples over water (", round(y_star/input$N,2), ").")
      )
    
    plot_list <- list(prior_post_null_plot, lklhd_null_plot)
    if (!is.null(input$curve)) {
      if (any(input$curve == "likelihood")) {
        plot_list[[2]] <- lklhd_plot
      }
      if (("prior" %in% input$curve) & !("posterior"  %in% input$curve)) {
        plot_list[[1]] <- prior_plot
      }
      if (("prior" %in% input$curve) & ("posterior"  %in% input$curve)) {
        plot_list[[1]] <- prior_post_plot
      }
      if (!("prior" %in% input$curve) & ("posterior"  %in% input$curve)) {
        plot_list[[1]] <- post_plot
      }
    }
    plot_grid(plotlist = plot_list, nrow = 1)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)