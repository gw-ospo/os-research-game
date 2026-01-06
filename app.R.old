library(shiny)
library(sortable)
library(htmltools)
library(yaml)

# ============================================================================
# LOAD CONFIGURATION FROM YAML FILE
# ============================================================================

CONFIG_FILE <- "config.yaml"

# Load and validate configuration
if (!file.exists(CONFIG_FILE)) {
  stop(sprintf("Configuration file not found: %s\nPlease create config.yaml in the same directory as app.R", CONFIG_FILE))
}

config <- yaml::read_yaml(CONFIG_FILE)

# Extract configuration sections
app_config <- config$app
bins <- config$bins
items <- config$items

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Create item HTML
create_item <- function(item) {
  # Check if logo is an image file or emoji
  if (grepl("\\.(png|jpg|jpeg|gif|svg)$", item$logo, ignore.case = TRUE)) {
    logo_path <- item$logo
    logo_html <- sprintf('<img src="%s" class="item-logo-img" alt="%s" onerror="this.style.display=\'none\'; this.nextElementSibling.style.display=\'block\';">', 
                        logo_path, item$name)
    fallback <- sprintf('<span class="item-logo" style="display:none;">%s</span>', 
                       substr(item$name, 1, 1))
    logo_html <- paste0(logo_html, fallback)
  } else {
    logo_html <- sprintf('<span class="item-logo">%s</span>', item$logo)
  }
  
  # Create initial hover tooltip with name and description
  initial_tooltip <- sprintf('<div class="initial-hover-tooltip"><strong>%s</strong><br/>%s</div>', 
                            item$name, item$description)
  
  HTML(sprintf(
    '<div class="item-box" data-id="%s" data-name="%s" data-description="%s" data-explanation="%s">
      %s
      %s
    </div>',
    item$id, 
    htmltools::htmlEscape(item$name), 
    htmltools::htmlEscape(item$description),
    htmltools::htmlEscape(if(!is.null(item$explanation)) item$explanation else ""),
    logo_html,
    initial_tooltip
  ))
}

# Generate JavaScript for tracking items in bins
generate_bin_js <- function(bins) {
  bin_code <- paste(sapply(bins, function(bin) {
    sprintf("
          var %sItems = [];
          $('#%s .item-box').each(function() {
            %sItems.push($(this).data('id'));
          });
          Shiny.setInputValue('%s_items_js', %sItems);",
            bin$id, bin$id, bin$id, bin$id, bin$id)
  }), collapse = "\n")
  return(bin_code)
}

# Generate JavaScript for highlighting results
generate_highlight_js <- function(bins) {
  highlight_code <- paste(sapply(bins, function(bin) {
    sprintf("
        if (message.%s) {
          message.%s.forEach(function(item) {
            var element = $('#%s .item-box[data-id=\"' + item.id + '\"]');
            element.addClass(item.is_correct ? 'correct' : 'incorrect');
            element.addClass('results-shown');
            element.append(item.explanation);
            
            // Add hover tooltip for incorrect answers
            if (!item.is_correct && item.hover_text) {
              var tooltip = '<div class=\"result-hover-tooltip\"><strong>' + 
                           element.data('name') + '</strong><br/>' + 
                           item.hover_text + '</div>';
              element.append(tooltip);
            }
            
            // Hide the initial tooltip
            element.find('.initial-hover-tooltip').remove();
          });
        }",
            bin$id, bin$id, bin$id)
  }), collapse = "\n")
  return(highlight_code)
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
        margin-bottom: 20px;
      }
      .main-title {
        flex: 1;
      }
      .branding {
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 14px;
        color: #555;
      }
      .branding a {
        color: #555;
        text-decoration: none;
        transition: color 0.3s;
      }
      .branding a:hover {
        color: #007bff;
      }
      .branding-logo {
        height: 40px;
        width: auto;
      }
      .item-box {
        padding: 15px;
        margin: 5px;
        background: white;
        border: 2px solid #ddd;
        border-radius: 8px;
        cursor: move;
        text-align: center;
        transition: all 0.3s;
        position: relative;
      }
      .item-box:hover {
        border-color: #4CAF50;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        z-index: 100;
      }
      .item-logo {
        font-size: 40px;
        display: block;
        margin-bottom: 5px;
      }
      .item-logo-img {
        width: 120px;
        height: 120px;
        object-fit: contain;
        display: block;
        margin: 0 auto 5px auto;
      }
      .item-name {
        font-weight: bold;
        font-size: 14px;
        display: none;
      }
      .item-description {
        font-size: 11px;
        color: #666;
        font-style: italic;
        margin-top: 3px;
        display: none;
      }
      .rank-list-container {
        min-height: 200px;
        padding: 20px;
        background: #f5f5f5;
        border: 3px dashed #ccc;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .bin-title {
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 15px;
        color: #333;
      }
      .correct {
        border-color: #4CAF50 !important;
        background: #e8f5e9 !important;
      }
      .incorrect {
        border-color: #f44336 !important;
        background: #ffebee !important;
      }
      .explanation {
        background: #fff3cd;
        border: 1px solid #ffc107;
        border-radius: 5px;
        padding: 8px;
        margin-top: 5px;
        font-size: 12px;
        color: #856404;
      }
      
      /* Initial hover tooltip (before submit) */
      .initial-hover-tooltip {
        display: none;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background: rgba(52, 73, 94, 0.75);
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-size: 13px;
        width: 95%;
        height: 95%;
        z-index: 10000;
        box-shadow: 0 8px 24px rgba(0,0,0,0.5);
        white-space: normal;
        text-align: center;
        line-height: 1.5;
        pointer-events: none;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        backdrop-filter: blur(2px);
      }
      .item-box:not(.results-shown):hover .initial-hover-tooltip {
        display: flex;
      }
      
      /* Result hover tooltip (after submit, for incorrect answers) */
      .result-hover-tooltip {
        display: none;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background: rgba(192, 57, 43, 0.75);
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-size: 13px;
        width: 95%;
        height: 95%;
        z-index: 10000;
        box-shadow: 0 8px 24px rgba(0,0,0,0.5);
        white-space: normal;
        text-align: center;
        line-height: 1.5;
        pointer-events: none;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        backdrop-filter: blur(2px);
      }
      .incorrect.results-shown:hover .result-hover-tooltip {
        display: flex;
      }
    ")),
    tags$script(HTML(sprintf("
      $(document).on('shiny:connected', function() {
        $('#submit').on('click', function() {
          %s
        });
      });
      
      Shiny.addCustomMessageHandler('highlight_results', function(message) {
        $('.item-box').removeClass('correct incorrect');
        $('.explanation').remove();
        
        %s
        
        if (message.score) {
          $('#score_display').html(message.score).show();
        }
      });
    ", generate_bin_js(bins), generate_highlight_js(bins))))
  ),
  
  titlePanel(
    div(class = "title-container",
        div(class = "main-title", app_config$title),
        if (!is.null(app_config$branding)) {
          div(class = "branding",
              a(href = app_config$branding$url, 
                target = "_blank",
                app_config$branding$text),
              if (!is.null(app_config$branding$logo)) {
                a(href = app_config$branding$url,
                  target = "_blank",
                  img(src = app_config$branding$logo, 
                      class = "branding-logo",
                      alt = "Logo"))
              }
          )
        }
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Instructions"),
      p(app_config$instructions),
      p(app_config$instructions_detail),
      actionButton("submit", app_config$submit_button_text, 
                   class = "btn-primary btn-lg", 
                   style = "width: 100%; margin-top: 20px;"),
      actionButton("reset", app_config$reset_button_text, 
                   class = "btn-secondary", 
                   style = "width: 100%; margin-top: 10px;"),
      
      hr(),
      
      div(class = "bin-title", "Available Items"),
      bucket_list(
        header = NULL,
        group_name = "sorting_group",
        orientation = "horizontal",
        add_rank_list(
          text = "",
          labels = lapply(items, create_item),
          input_id = "available_items"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      
      div(id = "score_display", 
          style = "display:none; margin-bottom: 20px; padding: 15px; background: #e3f2fd; border-radius: 8px; font-size: 18px; font-weight: bold; text-align: center;"),
      
      fluidRow(
        lapply(bins, function(bin) {
          column(
            12 / length(bins),
            div(class = "bin-title", bin$title),
            bucket_list(
              header = NULL,
              group_name = "sorting_group",
              orientation = "horizontal",
              add_rank_list(
                text = "",
                labels = list(),
                input_id = bin$id
              )
            )
          )
        })
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    # Collect items from each bin
    bin_contents <- lapply(bins, function(bin) {
      input_name <- paste0(bin$id, "_items_js")
      ids <- input[[input_name]]
      if (is.null(ids)) ids <- character(0)
      list(bin_id = bin$id, bin_title = bin$title, item_ids = ids)
    })
    
    # Build results for each bin
    all_results <- list()
    total <- 0
    correct <- 0
    
    for (bin_info in bin_contents) {
      bin_results <- list()
      
      for (item_id in bin_info$item_ids) {
        # Find the item
        item_index <- which(sapply(items, function(x) x$id == item_id))
        if (length(item_index) == 0) next
        
        item <- items[[item_index]]
        total <- total + 1
        
        is_correct <- item$correct_bin == bin_info$bin_id
        if (is_correct) correct <- correct + 1
        
        # Find correct bin title
        correct_bin_index <- which(sapply(bins, function(x) x$id == item$correct_bin))
        correct_bin_title <- bins[[correct_bin_index]]$title
        
        explanation <- if (!is_correct) {
          sprintf('<div class="explanation">%s</div>', 
                  sprintf(app_config$incorrect_message, correct_bin_title))
        } else {
          sprintf('<div class="explanation">%s</div>', app_config$correct_message)
        }
        
        # Get custom hover explanation if item is incorrect
        hover_text <- if (!is_correct && !is.null(item$explanation)) {
          item$explanation
        } else {
          NULL
        }
        
        bin_results[[length(bin_results) + 1]] <- list(
          id = item$id,
          is_correct = is_correct,
          explanation = explanation,
          hover_text = hover_text
        )
      }
      
      all_results[[bin_info$bin_id]] <- bin_results
    }
    
    score_pct <- if (total > 0) round(correct / total * 100) else 0
    score_html <- sprintf(app_config$score_message, correct, total, score_pct)
    
    # Send results to JavaScript for highlighting
    message <- c(all_results, list(score = score_html))
    
    session$sendCustomMessage(
      type = 'highlight_results',
      message = message
    )
  })
  
  observeEvent(input$reset, {
    session$reload()
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
