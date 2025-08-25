# Loading libraries 
library(shiny)
library(tidyverse)
library(shinyjs)
library(bslib)
library(janitor)
library(metathis)

# Setting the seed
set.seed(1)

# Importing the data
data <- read_csv(here::here("df.csv"))

# Adding a unique identifier for each row
data$x1 <- sample(seq(10000000, 99999999), nrow(data))

# Defining the UI
ui <- fluidPage(
  
  # Title that appears on the Tab
  title = "Policing in Louisiana, By The Facts | ACLU of Louisiana",
  
  
  # Defining the head
  tags$head(
    
    
    # Creating website Metadata
    tags$link(rel = "shortcut icon", href = "https://www.laaclu.org/profiles/aclu_affiliates/themes/custom/affiliates/favicons/favicon.ico?v=3.0"),
    tags$meta(property = "og:title", content = "Policing in Louisiana, By The Facts | ACLU of Louisiana"),
    tags$meta(property = "og:description", content = "Ask, answer, and share questions about policing in Louisiana"),
    tags$meta(property = "og:image", content = "https://www.aclujusticelab.org/wp-content/uploads/2020/12/ACLULA_JusticeLabStyleGuide-02.png"),
    tags$meta(property = "og:url", content = "https://laaclu.shinyapps.io/facts/"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:author", content = "Elijah Appelson"),
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:title", content = "Policing in Louisiana, By The Facts | ACLU of Louisiana"),
    tags$meta(name = "twitter:description", content = "Ask, answer, and share questions about policing in Louisiana"),
    tags$meta(name = "twitter:image", content = "https://www.aclujusticelab.org/wp-content/uploads/2020/12/ACLULA_JusticeLabStyleGuide-02.png"),
    
    
    # ------------------------- Defining the JavaScript ----------------------------
    
    # JavaScript
    tags$script(
      HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const restContent = document.getElementById('restContent');
      const mainScreen = document.getElementById('mainScreen');
      const btn = document.getElementById('continueBtn');

      if (restContent && mainScreen) {
        Object.assign(restContent.style, {
          position: 'absolute',
          top: 0,
          left: 0,
          width: '100%',
          height: '100%',
          zIndex: '1',
          visibility: 'hidden',
          opacity: 0
        });

        Object.assign(mainScreen.style, {
          position: 'relative',
          zIndex: '2',
          visibility: 'visible',
          opacity: 1,
          transition: 'all 0.3s ease'
        });
      }

      if (btn) {
        btn.addEventListener('click', function() {
          if (mainScreen && restContent) {
            Object.assign(mainScreen.style, {
              opacity: 0,
              visibility: 'hidden',
              zIndex: '1'
            });

            Object.assign(restContent.style, {
              visibility: 'visible',
              opacity: 1,
              zIndex: '2',
              transition: 'all 0.5s ease'
            });
          }
        });
      }
    });

    function copyToClipboard(text) {
      const textArea = document.createElement('textarea');
      textArea.value = text;
      document.body.appendChild(textArea);
      textArea.select();
      document.execCommand('copy');
      document.body.removeChild(textArea);
    }

    function disableButtonsTemporarily() {
      document.querySelectorAll('.normal-button').forEach(btn => btn.disabled = true);
      setTimeout(() => {
        document.querySelectorAll('.normal-button').forEach(btn => btn.disabled = false);
      }, 300);
    }

    $(document).ready(function() {
      $('#help_button').click(function() {
        $('.help-box').toggleClass('visible');
      });

      $('#question1, #question2, #question3').select2({
        placeholder: 'Search for a question',
        width: 'auto'
      }).on('select2:open', function() {
        $(this).data('select2').$dropdown.addClass('select2-dropdown');
      });

      $('#sort, #group').select2({
        placeholder: 'Search for a question',
        width: 'auto',
        minimumResultsForSearch: Infinity
      }).on('select2:open', function() {
        $(this).data('select2').$dropdown.addClass('select2-dropdown');
      });

      $('#question1, #question2, #question3').parent().find('span.select2-selection').hover(
        function() {
          $(this).css({
            'background-color': '#FEF98B',
            'color': 'black',
            'transform': 'scale(1.02)',
            'transition': 'all 0.5s ease'
          });
        },
        function() {
          $(this).css({
            'background-color': '#FFFFFF',
            'color': '',
            'transform': '',
            'transition': 'all 0.5s ease'
          });
        }
      );

      $('#sort, #group').parent().find('span.select2-selection').hover(
        function() {
          $(this).css({
            'background-color': '#FEF98B',
            'color': 'black',
            'transform': 'scale(1.05)',
            'transition': 'all 0.5s ease',
            'margin-bottom': '-5px'
          });
        },
        function() {
          $(this).css({
            'background-color': '#FFFFFF',
            'color': '',
            'transform': '',
            'transition': 'all 0.5s ease',
            'margin-bottom': '-5px'
          });
        }
      );
    });
  ")
    )),
  
  
  # ----------------------------- Adding the CSS ---------------------------------
  
  # Adding the social media buttons
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js"),
  
  # Defining the website style
  tags$style(
    HTML(
      "
  /* Creating the download data*/
  #downloadData {
          display: block;
    margin: 0 auto;
    margin-top: 0px;
    width: 50%;
    padding: 10px;
    margin-bottom: 0px;
      background-color: #0055AA;
      color: white;
      text-align: center;
      text-decoration: none;
      font-size: 16px;
      border-radius: 15px;
      border: none;
      cursor: pointer;
      transition: background-color 0.3s;
  }
      
  /* Creating a download data button style*/
  #downloadData:active  {
      background-color: #ADD8E6;
      box-shadow: inset 0 0 4px 4px rgba(173, 216, 230, 1)
  }
      
  /* Importing GT America font compressed regular */
  @font-face {
      font-family: 'gtam';
      src: url('https://static.aclu.org/fonts/GT-America-Compressed-Regular.woff2') format('woff2');
      font-weight: bold;
  }
  
  /* Importing GT America font compressed bold */
  @font-face {
      font-family: 'gtam2';
      src: url('https://static.aclu.org/fonts/GT-America-Compressed-Bold.woff2') format('woff2');
      font-weight: bold;
  }
  
  /* Importing Century regular font */
  @font-face {
      font-family: 'century';
      src: url('https://static.aclu.org/fonts/CenturySchoolbook-Regular-webfont.woff') format('woff');
  }
  
  /* Applying Century font */
  .help-box, body, .rounded-box{
      font-family: 'century'
  }
  
  /* Applying GT America font */
  .ribbon, .social-links a,.source-text {
      font-family: 'gtam' !important;
  }
  
  /* Adding checkbox margin */
  #saved_checkboxes {
      margin-top: 20px;
  }
  
  /* Defining the ribbon at the top of the boxes */
  .ribbon {
    position: relative;
    top: -40px; 
    left: 50%; 
    width: 400px;
    transform: translateX(-50%);
    text-align: center; 
    background-color: #FCAA17; 
    border-top-left-radius: 25px; 
    border-top-right-radius: 25px;
    color: #FFFFFF; 
    font-weight: bold;
    padding: 10px; 
    z-index: 1;
    font-size: 25px;
    transition: all 0.3s ease; 
  }
  
  
  /* Ribbon color for misconduct */
  .ribbon-misconduct {
    background-color: #ef404e;
  }
  
  /* Ribbon color for police killing */
  .ribbon-killing {
    background-color: #130f54;
  }
  
  /* Ribbon color for personnel */
  .ribbon-personnel {
    background-color: #fcaa17; 
  }
  
  /* Defining the rounded answer box */
  .rounded-box {
    position: relative; 
    z-index: 1;
    background-image: url('https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQTL2zA7fqOjSwqidu1i05tM5sSZjVIbAlSDpXtHX0I0g&s'), linear-gradient(to bottom right, #0055AA, #0055AA);
    background-size: 70px auto, cover;
    background-position: right bottom, center;
    background-repeat: no-repeat;
    border-radius: 25px; 
    padding: 40px 40px 40px; 
    width: 400px; 
    font-size: 25px;
    text-align: left;
    font-weight: normal;
    margin: 20px;
    box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);
    color: #FFFFFF; 
    transition: all 0.3s ease; 
    margin-bottom: 40px; 
    font-family: 'century';
  }
  
  /* Hover effect for the boxes */
  .rounded-box:hover {
    transform: scale(1.10);
    box-shadow: 0px 0px 16px  #FEF98B; 
    transition: all 0.5s ease; 
    filter: brightness(115%);
  }
  
  /* Shiny notification styling */
  .shiny-notification {
      background-color: #FCAA17; 
      font-family: 'gtam';
      color: white; 
      border-radius: 5px;
      padding: 10px; 
      font-size: 25px; 
      z-index: 9999;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.5);
      border: none;
      opacity: 0.90;
  
  }
  
  /* Common button styling */
  .normal-button-1,
  .normal-button-2,
  .normal-button-3 {
    font-size: 30px;
    margin-top: -10px;
    border-width: 0;
    transition: transform 0.3s ease, filter 0.3s ease;
  }
  
  /* Save button styling */
  .normal-button-1 {
    font-weight: bold;
    color: #025800;
  }
  
  /* Save button styling when hovering */
  .normal-button-1:hover {
    color: #012101;
    background-color: transparent;
    transform: scale(1.2);
    transition: all 0.5s ease;
  }
  
  /* Random button styling */
  .normal-button-2 {
    color: #0055AA;
  }
  
  /* Random button styling when hovering */
  .normal-button-2:hover {
    color: #001326;
    background-color: transparent;
    transform: scale(1.2);
    transition: all 0.5s ease;
  }
  
  /* Clear button styling */
  .normal-button-3 {
    color: #ef404e;
  }
  
  /* Clear button styling when hovering */
  .normal-button-3:hover {
    color: #260204;
    background-color: transparent;
    transform: scale(1.2);
    transition: all 0.5s ease;
  }
  
  /* Social media links */
  .social-links {
    position: absolute;
    top: 45%;
    right: 10px;
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    color: #FCAA17;
  }
  
  /* Individual social media link */
  .social-links a {
    color: #FCAA17; 
  }
  
  /* Individual social link when hovering */
  .social-links a:hover {
    color: #FED07E;
    transform: scale(1.25);
    transition: all 0.5s ease; 
  }
  
  /* Defining source-text */
  .source-text {
    color: #FCAA17; 
    font-size: 16px;
    font-style: italic;
    padding-right: 20px;
  }
  
  /* Defining source-text when hovering */
  .source-text:hover {
    color: #FEC051; 
  }
  
  /* Defining the inner box around the checkbox */
  .inner-box {
    background-color: white; 
    padding-bottom: 0px; 
    padding-left: 10px; 
    padding-right: 10px;
    border-radius: 20px;
    font-size:25px;
    font-family: 'gtam', sans-serif;
  }
  
  /* Defining the outer box around the checkbox */
  .outer-box {
    background-color: #fcaa17;
    text-align: left; 
    border-radius: 20px; 
    box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2); 
    margin-top: 20px;
    padding-bottom: 0px;
    font-family: 'gtam', sans-serif;
  }
  
  /* Defining the output box container */
  .column-container {
    display: flex;
    flex-wrap: wrap;
    justify-content: space-between;
  }
  
  /* Removing the border around selected choices */
  select:focus {
    outline: none; 
  }
  
  /* Fixing the checkbox display */
  .custom-checkbox-group .shiny-input-container {
      display: flex;
      flex-direction: column;
  }
  
  /* Centering checkbox display */
  .custom-checkbox-group .shiny-input-container .checkbox label {
      display: flex;
      align-items: center;
  }
  
  /* Moving the checkbox */
  .custom-checkbox-group .shiny-input-container .checkbox label input[type='checkbox'] {
      margin-right: 5px;
  }
  
  /* Styling of help-button*/
  .help-button {
      position: fixed;
      bottom: 20px;
      right: 20px;
      width: 70px; 
      height: 70px;
      background-color: #ef404e;
      color: white;
      border-style: solid;
      border-width: 0px;
      border-color: #ffffff;
      border-radius: 50%; 
      padding: 15px;
      font-size: 30px;
      box-shadow: 0px 0px 16px rgba(0, 0, 0, 0.2);
      cursor: pointer;
      z-index: 3000;
      transition: transform 0.3s ease, box-shadow 0.3s ease;
  }
  
  /* Styling the help button box */
  .help-box {
      position: fixed;
      width: 35%;
      height: 85%;
      bottom: 60px; 
      right: 60px; 
      background-color: #F5F5F5;
      color: #000000;
      border-radius: 10px;
      box-shadow: 0 0 0 1000px rgba(0,0,0,0.5);
      visibility: hidden;
      opacity: 0;
      transition: opacity 0.3s ease;
      z-index: 2000;
      font-size: 16px;
      overflow-y: scroll;
  }
      
  /* Defining the hover effect for the help button */
  .help-button:hover {
      transform: scale(1.10);
      box-shadow: 0px 0px 16px  #FEF98B; 
      transition: all 0.5s ease; 
      filter: brightness(115%);
  }
  
  /* Creating a visibility variable to be used for the help button */
  .visible {
      visibility: visible;
      opacity: 1;
  }
   
  /* Adding padding to the content of the help box */
  .help-box-content {
    padding: 20px; 
  }
  
  /* Creating styling for the questions at the top of the page */
  .question-content {
    text-align: center; 
    font-size: 35px;
    font-weight: normal;
    transition: all 0.5s ease; 
    font-family: 'gtam2';
    margin-top: 80px;
    color: black;
  }
  
  /* Creating styling for the description below the questions at the top of the page */
  .how-use {
    font-size: 25px; 
    font-style: italic; 
    color: #747474;
    font-weight: normal;
    font-family: 'gtam' !important;
    background-color: white;
  }
  
  /* Creating styling for the model when an error occurs */
  .modal-title {
           font-family: 'gtam2';
          font-size: 35px;
          color: #0055AA;
          text-align: center;
          font-weight: bold;
  }
      
  /* Creating styling for the model text when an error occurs */
  .modal-body {
          font-family: 'century';
          font-size: 16px;
  }
  
  /* Creating a container to organize the save, random, and clear buttons */
  .button-container {
      display: flex;
      justify-content: center;
      gap: 20px;
  }

  /* Adding text underneith save, random, and clear buttons */
  .normal-button {
      font-size: 30px;
      background-color: transparent;
      border-width: 0px;
      font-weight: bold;
      text-align: center;
      display: flex;
      flex-direction: column;
      align-items: center;
      transition: transform 0.3s ease, filter 0.3s ease;
  }
  
  /* Making the save, random, and clear button icons larger */
  .normal-button .fa {
      font-size: 30px;
  }
  
  /* Defining the the save, random, and clear button font */
  .normal-button span {
      font-size: 18px; 
      margin-top: 5px;
      display: block;
  }
  
  /* Downloader in help-box */
  .help-box-downloader {
      font-size:30px; 
      font-family: 'gtam2
  }
  
  /* Help box titles */
  .help-box-titles {
      font-size:30px; 
      font-family: 'gtam2';
  }
  
  /* Help box subtitles */
  .help-box-subtitles {
      font-size:20px; 
      font-family: 'gtam2';
  }
  
  /* Intro screen styling */
  .intro-screen {
      height: 100vh;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      font-size: 50px;
      font-weight: 700;
      color: #0055AA;
      text-align: center;
      font-family: 'gtam2';
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
      margin: 0;
      background-image: url('https://aclujusticelab.org/wp-content/uploads/2020/12/HomePage_HeroTexture.png');
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
  }
  
  /* Intro screen title styling */
  .intro-screen-title {
      color: black; 
      font-size: 56px; 
      font-weight: 900; 
      letter-spacing: 3px; 
      margin-bottom: 12px;
  }
  
  /* Intro screen subtitle styling */
  .intro-screen-subtitle {
      font-size: 40px; 
      font-weight: 400; 
      color: #0054AA; 
      font-style: italic; 
      margin-top: 0; 
      margin-bottom: 30px; 
      line-height: 1.2;
  }
  
  /* Intro screen continue button */
  .intro-screen-button {
      margin-top: 30px; 
      font-size: 20px; 
      padding: 10px 30px; 
      font-family: 'gtam2'; 
      font-weight: 600; 
      color: #fff; 
      background-color: #0055AA; 
      border: none; 
      border-radius: 6px; 
      cursor: pointer; 
      box-shadow: 2px 2px 6px rgba(0, 85, 170, 0.5);
  }
  
  /* Intro screen link container */
  .intro-screen-link-container {
      position: fixed; 
      bottom: 20px; 
      left: 20px; 
      display: flex; 
      flex-direction: column; 
      align-items: flex-end; 
      gap: 10px;
  }
  
  /* Intro screen link */
  .intro-screen-link {
      height: 70px; 
      width: auto; 
      cursor: pointer;
  }
  
  /* Main screen styling */
  .main-page {
      background-color: white;
      background-image: url('https://aclujusticelab.org/wp-content/uploads/2020/12/HomePage_HeroTexture.png');
      background-repeat: repeat;
  }

  /* Defining Select2 styling */
  .select2-container--default .select2-selection--single {
      border: none !important;
      margin-top: -10px; 
      height: auto;
      line-height: 2.5;
      background-color: white;
  }
  
  /* Defining Select2 container */
  .select2-container--default .select2-selection--single .select2-selection__rendered {
      line-height: 1.6; 
      color: black;
      border-radius: 20px;
      text-decoration: underline;
      text-decoration-style: dotted;
      text-decoration-color: #FCAA17;
      text-decoration-size: 30px;
  }
    
  /* Defining Select2 dropdown */
  .select2-dropdown {
      border: none !important;
      box-shadow: 2px 2px 6px rgba(0,0,0,0.5); 
      font-size: 18px;
  }
  
  /* Changing Select2 arrow */
  .select2-selection__arrow {
      transform: scale(1.5) !important;
  }
  
  /* Sort-by styling */
  .sort-by {
      font-size: 25px; 
      font-weight: normal;
      font-family: 'gtam';
      top: -50px;
  }
  
  /* Sort-by box styling */
  .sort-by-box {
      font-size: 27px;
      background-color: white; 
      padding: 20px; 
      border-radius: 20px;
      font-family: 'gtam'
  }
  
## ---------------------------- Screen Size Styling ----------------------------

  /* Sidepanel change */
  @media only screen and (max-width: 786px) {
    .sidebarPanel {
      border: 1px solid black;
      border-radius: 30px;
      padding: 10px;
      border-width: 0px;
    }
  }
  
  /* Select2 max width*/
  @media screen and (min-width: 787px) {
    .select2-container--default .select2-selection--single .select2-selection__rendered {
      max-width: 600px;
    }
  }
  
  /* Defining rounded-box width and the ACLU symbol based on the screen size */
  @media only screen and (max-width: 450px) {
    .rounded-box {
      width: calc(100% - 40px); 
      background-size: 70px auto, cover;
    }
  }

  @media only screen and (min-width: 451px) {
    .rounded-box {
      width: calc(100% - 40px); 
      background-size: 70px auto, cover;
    }
  }

  @media only screen and (min-width: 1001px) {
    .rounded-box {
      width: calc(50% - 40px); 
      background-size: 70px auto, cover;
    }
  }
  
  @media only screen and (min-width: 1400px) {
    .rounded-box {
        background-size: 70px auto, cover;
    }
  }
  
  @media only screen and (min-width: 1600px) {
    .rounded-box {
      width: calc(33.3333% - 40px);
    }
  }

  /* Defining ribbon width based on the screen size */
  @media only screen and (max-width: 600px) {
    .ribbon {
      width: calc(100% + 80px); 
    }
  }
  
  @media only screen and (min-width: 601px) {
    .ribbon {
      width: calc(100% + 80px);
    }
  }
  
  @media only screen and (min-width: 1200px) {
    .ribbon {
      width: calc(100% + 80px);
    }
  }
    
  /* Defining the select2 width based on the screen size */
  @media screen and (max-width: 786px) {
    .select2-container--default .select2-selection--single .select2-selection__rendered {
      max-width: 350px;
    }
  }
  
  /* Defining phone styling */
  @media screen and (max-width: 450px) {
  
    /* How to use */
    .how-use {
        font-size: 22px; 
        font-style: italic; 
        color: #747474;
        background-color: white;
        font-weight: normal;
        font-family: 'gtam' !important;
    }
      
    /* Question content */
    .question-content {
        text-align: center; 
        font-size: 30px;
        font-weight: bold;
        transition: all 0.5s ease; 
        font-family: 'gtam2';
        color: black;
    }
      
    /* Ribbon */
    .ribbon {
        font-size: 22px;
        transition: all 0.3s ease; 
        font-style: italic;
    }
  
    
    /* Rounded box hovering */
     .rounded-box:hover {
      transform: scale(1);
      box-shadow: 0px; 
      transition: all 0.5s ease; 
      filter: brightness(100%);
     }
     
     /* Help box */
    .help-box {
          bottom: 60px; 
          right: 20px;
          width: 90%; 
    }
    
    /* Save, Random, and Clear buttons */
    .normal-button-1:hover {
      transform: scale(1); 
      transition: none;
    }
    .normal-button-2:hover {
      transform: scale(1); 
      transition: none;
    }
    .normal-button-3:hover {
      transform: scale(1); 
      transition: none;
    }
    
    /* Social links */
    .social-links {
      top: 50%;
    }
    
    /* Removing sort and group */
    #sort_group_wrapper {
          display: none;
    }
      
    /* Changing the inner box */
    .inner-box {
      background-color: white; 
      padding-bottom: 10px; 
      padding-top: 10px; 
      padding-left: 10px; 
      padding-right: 10px;
      border-radius: 20px;
      font-size:25px;
      font-family: 'gtam', sans-serif;
    }
    
    /* Changing the outer box */
    .outer-box {
      background-color: #fcaa17;
      text-align: left; 
      border-radius: 20px; 
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2); 
      margin-top: 0px;
      padding-bottom: 20px;
      font-family: 'gtam', sans-serif;
      position: absolute; /* Removes it from the document flow */
      opacity: 0; /* Makes it invisible */
      height: 0; /* Ensures no space is taken up */
      overflow: hidden; /* Prevents any content from affecting layout */
    }
  }
  
  .modal-disclaimer .modal-header {
        background-color: #ffa500;
        color: white;
        font-weight: bold;
      }
      .modal-disclaimer .modal-body {
        background-color: #fffaf0;
        font-size: 14px;
        line-height: 1.6;
        color: #333;
      }
      .modal-disclaimer .modal-footer {
        background-color: #fffaf0;
      }
  "
    )
  ),
  
  # ------------------------ Defining the Help Box -------------------------------
  
  # Adding the help button
  tags$button(
    id = "help_button",
    class = "help-button",
    tags$i(class = "fa fa-question")
  ),
  div(
    class = "help-box",
    div(class = "help-box-content",
        tags$p("Download", class="help-box-titles"),
        tags$p("Click the button below to download your filtered data.")
    ),
    downloadButton("downloadData", ""),
    div(class = "help-box-content",
        tags$p("Summary", class="help-box-titles"),
        tags$p("This comprehensive project contains 55 unique questions regarding police killings, misconduct, and or personnel from 330+ law 
               enforcement agencies across Louisiana spanning 60+ years, for a total of 120,000+ facts. We created this project to make
               actionable insights easy to search, find, and share."),
        
        tags$br(),
        
        tags$p("Sources", class="help-box-titles"),
        tags$p("The data sources used in this tool include the ", 
               tags$a(href = "https://llead.co/", "Louisiana Law Enforcement and Accountability Database"), "(Updated February 10th, 2024), ",
               tags$a(href = "https://mappingpoliceviolence.org/", "Mapping Police Violence"), "(Updated July 28th, 2025), and the ",
               tags$a(href = "https://cde.ucr.cjis.gov/", "FBI Crime Explorer Law Enforcement Personnel Data"), "(Updated February 17th, 2025)."),
        tags$br(),
        
        tags$p("Usage", class="help-box-titles"),
        tags$p("Generating a Question", class="help-box-subtitles"),
        tags$p("You can either generate a question using the 'random' button or select a question using the three categories at the top of the tool and selecting the 'save' button."),
        
        tags$br(),
        
        tags$p("Sorting Your Questions", class="help-box-subtitles"),
        tags$p("You can sort your saved questions by using the 'sort by' button in the left-hand-side of the tool. The sort button orders the saved questions alphabetically (A-Z) for 'Question' and 'Agency' and from high to low for 'Year'."),
        
        tags$br(),
        
        tags$p("Deselecting a Question",class="help-box-subtitles"),
        tags$p("You can deselect a question by clicking its check-box on the left-hand-side of the tool."),
        
        tags$br(),
        
        tags$p("Removing all Questions",class="help-box-subtitles"),
        tags$p("You can clear all saved questions by clicking the 'remove' icon at the top of the tool."),
        
        tags$br(),
        
        tags$p("Disclaimer", class="help-box-titles"),
        tags$p(
          div(
            "The information provided by this tool is generated using publicly available data from the",
            tags$a(href = "https://llead.co/", "Louisiana Law Enforcement and Accountability Database,"),
            tags$a(href = "https://mappingpoliceviolence.org/", "Mapping Police Violence,"),"and the",
            tags$a(href = "https://cde.ucr.cjis.gov/", "FBI Crime Explorer Law Enforcement Personnel Data"),
            "and may not be fully accurate or complete. ",
            "Misconduct allegation categories, such as use of force, domestic violence, and others, were inferred through predictive methods applied to the misconduct allegation data.",
            "As a result, they may not reflect the full scope of misconduct or violence. ",
            "Additionally, significant limitations exist due to reporting issues by law enforcement agencies. For example, it is not possible to accurately determine the number of domestic violence allegations, ",
            "as RS 40:2533 allows law enforcement to expunge such records. ",
            tags$em("Please use this tool with caution, understanding that the data is inherently imperfect.")
          )
        ),
        
        tags$br(),
        
        tags$p("Questions", class="help-box-titles"),
        tags$p("If you have any questions or concerns about the content of this tool, you can contact ", tags$a(href = "mailto:eappelson@laaclu.org", "eappelson@laaclu.org."), " To learn more about how the tool was built, you can visit this project’s ", tags$a(href = "https://github.com/aclu-national/LA_police_facts", "GitHub Page."))
    )
  ),
  
  
  # ------------------- Defining the Introductory Screen UI  ---------------------
  
  # Defining the UI
  tags$div(
    id = "mainScreen",
    class = "intro-screen",
    tags$h1(
      "POLICING IN LOUISIANA,",
      class = "intro-screen-title"
    ),
    tags$h2(
      "BY THE FACTS",
      class = "intro-screen-subtitle"
    ),
    tags$button(
      id = "continueBtn",
      "Continue",
      class = "intro-screen-button"
    ),
    tags$div(
      class = "intro-screen-link-container",
      tags$a(
        href = "https://aclujusticelab.org",
        target = "_blank",
        tags$img(
          src = "https://aclujusticelab.org/wp-content/uploads/2020/12/ACLULA_JusticeLabStyleGuide-02.png",
          class = "intro-screen-link"
        )
      )
    ),
    tags$div(
      class = "intro-screen-link-container",
      tags$a(
        href = "https://aclujusticelab.org",
        target = "_blank",  # optional, opens link in new tab
        tags$img(
          src = "https://aclujusticelab.org/wp-content/uploads/2020/12/ACLULA_JusticeLabStyleGuide-02.png",
          class = "intro-screen-link"
        )
      )
    )
  ),
  
  # ------------------------- Defining the Main Page UI  -------------------------
  
  tags$div(
    id = "restContent",
    class = "main-page",
    
    tags$br(),
    
    # Questions at Top
    tags$div(
      class = "question-content",
      
      tags$p(
        # Question 1 select
        tags$select(
          id = "question1",
          lapply(c("All Questions", unique(data$question_p1)), function(option) {
            tags$option(option, value = option)
          })
        ),
        
        # Question 2 select
        tags$select(
          id = "question2",
          lapply(c("All Agencies", unique(data$question_p2)), function(option) {
            tags$option(option, value = option)
          })
        ),
        
        "in",
        
        # Question 3 select
        tags$select(
          id = "question3",
          lapply(c("All Years", unique(data$question_p3)), function(option) {
            tags$option(option, value = option)
          })
        ),
        
        "?"
      ),
      
      # How to use instructions
      tags$p(
        'Select a question, agency, and year by clicking the drop-downs above. Press the "Submit" button to see the answer below.',
        br(),
        class = "how-use"
      ),
      
      # Buttons: Save, Random, Clear
      tags$div(
        class = "button-container",
        
        actionButton(
          "save_button",
          HTML('<i class="fas fa-paper-plane"></i><span>Submit</span>'),
          class = "normal-button normal-button-1",
          onclick = "disableButtonsTemporarily()"
        ),
        
        actionButton(
          "random_button",
          HTML('<i class="fas fa-dice"></i><span>Randomize</span>'),
          class = "normal-button normal-button-2",
          onclick = "disableButtonsTemporarily()"
        ),
        
        actionButton(
          "clear_button",
          HTML('<i class="fa fa-trash"></i><span>Clear</span>'),
          class = "normal-button normal-button-3",
          onclick = "disableButtonsTemporarily()"
        )
      )
    ),
    
    tags$br(),
    
    # Sidebar Panel
    sidebarPanel(
      tags$div(
        id = "sort_group_wrapper",
        
        tags$div(
          class = "sort-by-box",
          
          tags$div(
            tags$span(
              "Sort by:",
              class = "sort-by"
            ),
            
            tags$select(
              id = "sort",
              lapply(c("None", "Question", "Agency", "Year"), function(option) {
                tags$option(option, value = option)
              })
            )
          )
        )
      ),
      
      # Saved checkboxes output
      tags$div(
        uiOutput("saved_checkboxes"),
        class = "inner-box"
      ),
      
      class = "outer-box"
    ),
    
    # Output boxes
    uiOutput("boxes")
  )
)

# -------------------------- Defining the Server  -----------------------------

# Defining the server
server <- function(input, output, session) {
  
  # Filtering data based on questions
  filtered_options <- reactive({
    df <- data
    if (input$question1 != "All Questions") {
      df <- filter(df, question_p1 == input$question1)
    } 
    if (input$question2 != "All Agencies") {
      df <- filter(df, question_p2 == input$question2)
    } 
    if (input$question3 != "All Years") {
      df <- filter(df, question_p3 == input$question3)
    }
    
    # Creating a list of all possibly questions based on the filterings
    list(
      q1 = unique(df$question_p1),
      q2 = unique(df$question_p2),
      q3 = unique(df$question_p3)
    )
  })
  filtered_choices <- reactive({
    df <- data
    
    # Initialize the questions with default values
    q1 <- unique(df$question_p1)
    q2 <- unique(df$question_p2)
    q3 <- unique(df$question_p3)
    
    # Apply filters based on inputs
    if (input$question1 != "All Questions") {
      q2 <- df %>% filter(question_p1 == input$question1) %>% pull(question_p2) %>% unique()
      q3 <- df %>% filter(question_p1 == input$question1) %>% pull(question_p3) %>% unique()
    }
    
    if (input$question2 != "All Agencies") {
      q1 <- df %>% filter(question_p2 == input$question2) %>% pull(question_p1) %>% unique()
      q3 <- df %>% filter(question_p2 == input$question2) %>% pull(question_p3) %>% unique()
    }
    
    if (input$question3 != "All Years") {
      q1 <- df %>% filter(question_p3 == input$question3) %>% pull(question_p1) %>% unique()
      q2 <- df %>% filter(question_p3 == input$question3) %>% pull(question_p2) %>% unique()
    }
    
    if (input$question1 != "All Questions" & input$question2 != "All Agencies") {
      q3 <- df %>% filter(question_p1 == input$question1,
                          question_p2 == input$question2) %>% pull(question_p3) %>% unique()
    }
    
    if (input$question1 != "All Questions" & input$question3 != "All Years") {
      q2 <- df %>% filter(question_p1 == input$question1,
                          question_p3 == input$question3) %>% pull(question_p2) %>% unique()
    }
    
    if (input$question2 != "All Agencies" & input$question3 != "All Years") {
      q1 <- df %>% filter(question_p2 == input$question2,
                          question_p3 == input$question3) %>% pull(question_p1) %>% unique()
    }
    
    list(q1 = q1, q2 = q2, q3 = q3)
  })
  
  # Updating the question options based on the choices selected
  observe({
    updateSelectInput(session, "question1", choices = c("All Questions",sort(filtered_choices()$q1)), selected = input$question1)
  })
  observe({
    updateSelectInput(session, "question2", choices = c("All Agencies",sort(filtered_choices()$q2)), selected = input$question2)
  })
  observe({
    updateSelectInput(session, "question3", choices = c("All Years",sort(filtered_choices()$q3, decreasing = TRUE)), selected = input$question3)
  })
  
  observe({
    showModal(modalDialog(
      title = "Disclaimer",
      easyClose = TRUE,
      footer = NULL,
      class = "modal-disclaimer",
      div(
        "The information provided by this tool is generated using publicly available data from the",
        tags$a(href = "https://llead.co/", "Louisiana Law Enforcement and Accountability Database,"),
        tags$a(href = "https://mappingpoliceviolence.org/", "Mapping Police Violence,"),"and the",
        tags$a(href = "https://cde.ucr.cjis.gov/", "FBI Crime Explorer Law Enforcement Personnel Data"),
        "and may not be fully accurate or complete. ",
        "Misconduct allegation categories, such as use of force, domestic violence, and others, were inferred through predictive methods applied to the misconduct allegation data.",
        "As a result, they may not reflect the full scope of misconduct or violence. ",
        "Additionally, significant limitations exist due to reporting issues by law enforcement agencies. For example, it is not possible to accurately determine the number of domestic violence allegations, ",
        "as RS 40:2533 allows law enforcement to expunge such records. ",
        tags$em("Please use this tool with caution, understanding that the data is inherently imperfect.")
      )
    ))
  })
  
  # Defining an empty list of saved filters
  saved_filters <- reactiveValues()
  
  # Defining the random button
  observeEvent(input$random_button, {
    
    showNotification("Question Saved.", type = "message", duration = 3)
    
    # Defining the random row function
    select_random_row <- reactive({
      random_index <- sample(nrow(data), 1)
      random_row <- data[random_index, ]
      return(random_row)
    })
    
    # Defining a function to pull questions from the random row
    random_questions <- reactive({
      random_row <- select_random_row()
      random_q1 <- random_row$question_p1
      random_q2 <- random_row$question_p2
      random_q3 <- random_row$question_p3
      return(list(q1 = random_q1, q2 = random_q2, q3 = random_q3))
    })
    
    # Updating the input based on the randomly selected rows
    observe({
      updateSelectInput(session, "question1", choices = c("All Questions",unique(data$question_p1)), selected = random_questions()$q1)
      updateSelectInput(session, "question2", choices = c("All Agencies",unique(data$question_p2)), selected = random_questions()$q2)
      updateSelectInput(session, "question3", choices = c("All Years",unique(data$question_p3)), selected = random_questions()$q3)
    })
    
    saved_filters[[paste0(random_questions()$q1," ", random_questions()$q2, " in ",random_questions()$q3,"?")]] <- TRUE
  })
  
  observeEvent(input$show_copy_notification, {
    showNotification("Text Copied.", type = "message", duration = 3)
  })
  
  # Defining the save button
  observeEvent(input$save_button, {
    
    if (input$question1 == "All Questions" & 
        input$question2 == "All Agencies" &
        input$question3 == "All Years") {
      showModal(modalDialog(
        title = "Incomplete Selection",
        'Only one question can be left as "All". Please select at least two of the following: a specific question, agency, or year before saving.',
        easyClose = TRUE,
        footer = NULL
      ))
      return()  
    }
    
    if (input$question1 == "All Questions" & 
        input$question2 == "All Agencies" &
        input$question3 != "All Years") {
      
      showModal(modalDialog(
        title = "Incomplete Selection",
        'Only one question can be left as "All". Please select a specific question, agency, or both before saving.',
        easyClose = TRUE,
        footer = NULL
      ))
      return()  
    }
    
    if (input$question1 == "All Questions" & 
        input$question2 != "All Agencies" &
        input$question3 == "All Years") {
      
      showModal(modalDialog(
        title = "Incomplete Selection",
        'Only one question can be left as "all". Please select a specific question, year, or both before saving.',
        easyClose = TRUE,
        footer = NULL
      ))
      return() 
    }
    
    if (input$question1 != "All Questions" & 
        input$question2 == "All Agencies" &
        input$question3 == "All Years") {
      
      showModal(modalDialog(
        title = "Incomplete Selection",
        'Only one question can be left as "all". Please select a specific agency, year, or both before saving.',
        easyClose = TRUE,
        footer = NULL
      ))
      return() 
    }
    
    # If question 1,2, and 3 are defined
    
    if (input$question1 != "All Questions" &
        input$question2 != "All Agencies" &
        input$question3 != "All Years"){
      saved_filters[[paste0(input$question1," ", input$question2, " in ",input$question3,"?")]] <- TRUE
      
      showNotification("Question Saved.", type = "message", duration = 3)
    }
    
    # If question 2 and 3 are defined
    if (input$question1 == "All Questions" &
        input$question2 != "All Agencies" &
        input$question3 != "All Years"){
      df_list <- data %>%
        filter(question_p2 == input$question2 &
                 question_p3 == input$question3) %>%
        pull(question_complex)
      for (question in df_list){
        saved_filters[[paste0(question)]] <- TRUE
      }
      showNotification("Questions Saved.", type = "message", duration = 3)
    }
    
    # If question 1 and 3 are defined
    if (input$question1 != "All Questions" &
        input$question2 == "All Agencies" &
        input$question3 != "All Years"){
      df_list <- data %>%
        filter(question_p1 == input$question1 &
                 question_p3 == input$question3) %>%
        pull(question_complex)
      for (question in df_list){
        saved_filters[[paste0(question)]] <- TRUE
      }
      showNotification("Questions Saved.", type = "message", duration = 3)
    }
    
    # If question 1 and 2 are defined
    if (input$question1 != "All Questions" &
        input$question2 != "All Agencies" &
        input$question3 == "All Years"){
      df_list <- data %>%
        filter(question_p1 == input$question1 &
                 question_p2 == input$question2) %>%
        pull(question_complex)
      for (question in df_list){
        saved_filters[[paste0(question)]] <- TRUE
      }
      showNotification("Questions Saved.", type = "message", duration = 3)
    }
    
  })
  
  # Defining the clear button 
  observeEvent(input$clear_button, {
    for (name in names(saved_filters)) {
      saved_filters[[name]] <- FALSE
      updateSelectInput(session, "question1", choices = c("All Questions",unique(data$question_p1)), selected = "All Questions")
      updateSelectInput(session, "question2", choices = c("All Agencies",unique(data$question_p2)), selected = "All Agencies")
      updateSelectInput(session, "question3", choices = c("All Years",unique(data$question_p3)), selected = "All Years")
    }
    
    showNotification("Questions Cleared.", type = "message", duration = 3)
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$id)) {
      id <- query$id
      
      selected_fact <- data[data$x1 == id, ]
      
      if (nrow(selected_fact) == 0) {
        sample_questions <- sample(data$question_complex, 1)
        for (question in sample_questions) {
          saved_filters[[question]] <- TRUE
        }
      } else {
        saved_filters[[selected_fact$question_complex]] <- TRUE
      }
    } else {
      # No id in the URL, display a random question
      sample_questions <- sample(data$question_complex, 1)
      for (question in sample_questions) {
        saved_filters[[question]] <- TRUE
      }
    }
  })
  
  
  # Creating the saved checkboxes based on the input
  output$saved_checkboxes <- renderUI({
    selected_filters <- Filter(function(x) saved_filters[[x]], names(saved_filters))
    div(
      class = "custom-checkbox-group",
      checkboxGroupInput(
        "saved_filters",
        label = "",
        choices = rev(selected_filters),
        selected = rev(selected_filters)
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_policing_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Filter data based on saved filters
      filtered_data <- filter(data, question_complex %in% input$saved_filters) %>%
        mutate(question_link = paste0("https://laaclu.shinyapps.io/facts/?id=",x1)) %>%
        select(
          Question = question_p1,
          Agency = question_p2,
          Year = question_p3,
          "Full Question" = question_complex, 
          Answer = value,
          "Full Answer" = text,
          Category = category,
          Source = source,
          "Source Link" = link,
          "Question Link" = question_link)
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  
  
  # Creating the output boxes
  output$boxes <- renderUI({
    
    # If no boxes are selected, do not return anything
    if (length(input$saved_filters) == 0) {
      div()
    } else {
      
      # Filtering our data to have the saved filters
      df <- filter(data,question_complex %in% c(input$saved_filters))
      
      df <- df %>%
        mutate(order = factor(question_complex, levels = input$saved_filters))
      
      
      if (input$sort == "None") {
        df <- df %>%
          ungroup() %>%
          arrange(order) %>%
          mutate(id = "")
      }
      
      # Defining hl sort with agency group
      if (input$sort == "Question"){
        df <- df %>%
          ungroup() %>%
          arrange(question_p1, desc(num_value)) %>%
          group_by(question_p1) %>%
          mutate(id = row_number()) 
      }
      
      # Defining lh sort with agency group
      if (input$sort == "Agency"){
        df <- df %>%
          ungroup() %>%
          arrange(correct_agency_name,desc(num_value)) %>%
          group_by(correct_agency_name) %>%
          mutate(id = row_number())
      }
      
      # Defining hl sort with question group
      if (input$sort == "Year"){
        df <- df %>%
          ungroup() %>%
          arrange(desc(year),desc(num_value)) %>%
          group_by(year) %>%
          mutate(id = row_number())
      }
      
      
      df <- df %>%
        mutate(
          
          #question_complex2 = str_to_upper(question_complex)
          question_complex2 = question_complex
          
        )
      
      # Creating the boxes
      boxes <- lapply(1:nrow(df), function(i) {
        
        id <- df$x1[i]
        
        # Answer source
        source <- df$source[i]
        
        # Answer link
        link <- df$link[i]
        
        # Answer text
        fact_text <- df$text[i]
        
        # Question
        question <- df$question_complex2[i]
        
        # Question category
        category_type <- df$category[i]
        
        # Ribbon class
        ribbon_class <- switch(category_type,
                               "Misconduct" = "ribbon-misconduct",
                               "Arrest" = "ribbon-arrest",
                               "Killing" = "ribbon-killing",
                               "Personnel" = "ribbon-personnel")
        
        # Defining the css of the rounded box
        rounded_box <- sprintf(
          
          '
    <div class="rounded-box">
      <div class="ribbon %s">%s</div> 
      %s
      <a href=%s target="_blank" class="source-text">
        <div class="source-text">Source: %s</div>
      </a>
      <div class="social-links">
        <a href="mailto:?subject=Policing in Louisiana&body=%s (via the ACLU of Louisiana) - https://laaclu.shinyapps.io/facts/?id=%s" target="_blank" style="color: #FCAA17;">
          <i class="fas fa-envelope"></i>
        </a>
        <a href="https://twitter.com/intent/post?url=https://laaclu.shinyapps.io/facts/?id=%s&text=%s (via @ACLUofLouisiana)" target="_blank" style="color: #FCAA17;">
          <i class="fab fa-twitter"></i>
        </a>
        <a href="javascript:void(0);" onclick="copyToClipboard(`%s (via the ACLU of Louisiana) - https://laaclu.shinyapps.io/facts/?id=%s`); Shiny.setInputValue(`show_copy_notification`, Math.random());">        
        <i class="fas fa-copy"></i>
        </a>
      </div>
    </div>
  ', 
          # %s inputs
          ribbon_class, question, fact_text, link, source, fact_text, id, id, fact_text,fact_text, id)
        
        # Outputting an HTML representation of each box
        HTML(rounded_box)
      })
      
      # Placing the boxes in the column container defined
      div(class = "column-container", boxes)
    }
  })
  
  
}

# Running the shiny app
shinyApp(ui = ui, server = server)
