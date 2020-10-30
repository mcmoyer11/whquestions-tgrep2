function make_slides(f) {
  var slides = {};

  slides.bot = slide({
    name: "bot",
    start: function () {
      $('.err1').hide();
      $('.err2').hide();
      $('.disq').hide();
      exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
      exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
      exp.lives = 0;
      var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
      var question = 'Who does ' + exp.speaker + ' talk to?';
      document.getElementById("s").innerHTML = story;
      document.getElementById("q").innerHTML = question;
    },
    button: function () {
      exp.text_input = document.getElementById("text_box").value;
      var lower = exp.listener.toLowerCase();
      var upper = exp.listener.toUpperCase();

      if ((exp.lives < 3) && ((exp.text_input == exp.listener) | (exp.text_input == lower) | (exp.text_input == upper))) {
        exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "tgrep_id": "bot_check",
          "response": [exp.text_input, exp.listener],
          "sentence": "",
        });
        exp.go();
      }
      else {
        exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "tgrep_id": "bot_check",
          "response": [exp.text_input, exp.listener],
          "sentence": "",
        });
        if (exp.lives == 0) {
          $('.err1').show();
        } if (exp.lives == 1) {
          $('.err1').hide();
          $('.err2').show();
        } if (exp.lives == 2) {
          $('.err2').hide();
          $('.disq').show();
          $('.button').hide();
        }
        exp.lives++;
      }
    },
  });

  slides.i0 = slide({
    name: "i0",
    start: function () {
      $("#n_trials").html(exp.n_trials);
      exp.startT = Date.now();
    }
  });

  slides.example1 = slide({
    name: "example1",

    start: function () {
      $('.err').hide();

      var contexthtml = "<b>Speaker #1</b>: I'm so hungry, let's order some food. I want a double cheeseburger, fries and a large shake. <br> <b>Speaker #2</b>: I'm not that hungry, "
      var entirehtml = "<font color=#FF0000> " + "I'll either get a burger or fries."
      contexthtml = contexthtml+entirehtml
      var besthtml = "<font color=#0000FF> " + "I'll either get a burger or fries <b>but not both</b>."

      $(".context").html(contexthtml);
      $(".BestResponse").html(besthtml);
      $(".err").hide();
   
    },

    button: function () {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio == "5" | this.radio == "6" | this.radio == "7") {
        this.log_responses();
        exp.go();
      }
      else {
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses: function () {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "tgrep_id": "example1",
        "response": [this.radio, ""],
        "sentence": "",
      });
    },
  });


  slides.example2 = slide({
    name: "example2",

    start: function () {
      $(".err").hide();

      var contexthtml = "<b>Speaker #1</b>: Do your employees speak any language besides English? <br> <b>Speaker #2</b>: "
      var entirehtml = "<font color=#FF0000> " + "All our employees also speak at least French or Spanish."
      contexthtml = contexthtml+entirehtml
      var besthtml = "<font color=#0000FF> " + "All our employees also speak at least French or Spanish <b>but not both</b>."

      $(".context").html(contexthtml);
      $(".BestResponse").html(besthtml);
      $(".err").hide();
 
    },
    button: function () {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio == "1" | this.radio == "2" | this.radio == "3") {
        this.log_responses();
        exp.go();
      }
      else {
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses: function () {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "tgrep_id": "example2",
        "response": [this.radio, ""],
        "sentence": "",
      });
    },
  });

  slides.startExp = slide({
    name: "startExp",
    start: function () {
      $("#instrunctionGen").html('<table class=table1 id="instructionGen"> </table>');
      var dispRow = $(document.createElement('tr')).attr("id", 'rowp' + 1);
      dispRow.append("<div class=row>");
      dispRow.append("<div align=center><button class=continueButton onclick= _s.button()>Continue</button></div>");
      dispRow.append('<td/>');
      dispRow.append('</div>');
      dispRow.appendTo("#instructionGen");

    },
    button: function () {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.generateEntities = slide({
    name: "generateEntities",
    present: exp.stimuli, // This the array generated from stimuli.js

    present_handle: function (stim) { // this function is called bascially on exp.stim (more or less)

      $("input[name='number']:checked").prop('checked', false);

      this.counter = 1;

      var generic = stim;
      this.generic = generic;
      this.response = response = false;

      var contexthtml = generic.PreceedingContext;
      // var contexthtml = this.format_context(generic.PreceedingContext);
      var entirehtml = "<font color=#FF0000> " + generic.EntireSentence
      contexthtml = contexthtml+entirehtml
      var indefhtml = generic.AResponse // This is where you might have to replace things

      $(".context").html(contexthtml);
      $(".AResponse").html(indefhtml);
      $(".err").hide();

      this.counter++;

      var checkbox = document.createElement('input'); 
      checkbox.setAttribute('align', 'center');
      exp.checkbox = document.getElementById("strange");

      $(".err").hide();

    },

    // speakers 1 and 2 
    format_context: function (context) {
      contexthtml = "<br><b>Speaker #1:</b>" + contexthtml;
      contexthtml = contexthtml.replace(/###/g, "");
      // replace first three ## with Speaker 1
      // contexthtml = contexthtml.replace($###/g, " "); Initial Speaker A
      contexthtml = context.replace("#+speakera(\d+).#+", "<br><b>Speaker #A:</b>"); // ###SpeakerA55-2*t55-2, SpeakerA53*t53
      contexthtml = contexthtml.replace("#+speakerb(\d+).#+", "<br><b>Speaker #B:</b>"); // ###SpeakerA53*t53
      // contexthtml = contexthtml.replace(/###SpeakerA(\d+)?-(\d+)?\*t(\d+)?-(\d+)? ./g, "<br><b>Speaker #1:</b>"); // ###SpeakerA55-2*t55-2, SpeakerA53*t53
      // contexthtml = contexthtml.replace(/###SpeakerANaN\*t(\d+)-(\d+) ./g, "<br><b>Speaker #1:</b>"); // ###SpeakerANaN*t55-1
      // contexthtml = contexthtml.replace(/###SpeakerB(\d+).t(\d+) ./g, "<br><b>Speaker #2:</b>"); // ###SpeakerB0*t60 
      // contexthtml = contexthtml.replace(/###SpeakerB(\d+)?\*t(\d+)? ./g, "<br><b>Speaker #2:</b>"); // ###SpeakerA55-2*t55-2, SpeakerA53*t53
      // contexthtml = contexthtml.replace(/###SpeakerB(\d+)?-(\d+)?\*t(\d+)?-(\d+)? ./g, "<br><b>Speaker #2:</b>"); // 
      // contexthtml = contexthtml.replace(/###SpeakerBNaN\*t(\d+)-(\d+) ./g, "<br><b>Speaker #2:</b>"); //###SpeakerBNaN*t57-2
      // contexthtml = contexthtml.replace(/E_S/g, "");
      // contexthtml = contexthtml.replace(/(\\\[| \\\+|\\\])/g, "");
      // contexthtml = contexthtml.replace(/-N((\d+)|[A-Z]+)+/g, "");
      
      
      // contexthtml = contexthtml.replace(/ ,/g, ",");
      // contexthtml = contexthtml.replace(/ \./g, ".");

      // This should help with getting the speaker ordering right
      // intended logic:

      // if (contexthtml.startsWith("<br><b>Speaker A")) {
        // Switch all instances of Speaker A with Speaker 2
        // and speaker B with Speaker 1
      // }
      //  elif (contexthtml.startsWith("<br><b>Speaker B")) {
            // switch all instances of Speaker B with Speaker 2
            // and Speaker A with Speaker 1
      //  }
      if (!contexthtml.startsWith("<br><b>Speaker #")) {
        var ssi = contexthtml.indexOf("Speaker #");
        switch (contexthtml[ssi + "Speaker #".length]) {
          case "1":
            contexthtml = "<br><b>Speaker #2:</b> " + contexthtml;
            break;
          case "2":
            contexthtml = "<br><b>Speaker #1:</b> " + contexthtml;
            break;
          default:
            break;
        }
      };
      return contexthtml;
    },

    button: function () {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio) {
        this.log_responses();
        $(this.radio).prop('checked', false);
        exp.checkbox.checked = false;
        //this.checked = false;
        _stream.apply(this);
      }
      else {
        $('.err').show();
        //this.log_responses();
      }
    },

    log_responses: function () {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "tgrep_id": this.generic.TGrep,
        "response": [this.radio, exp.checkbox.checked],
        "sentence": this.generic.BestResponse,
      });
    },

  });

  slides.subj_info = slide({
    name: "subj_info",
    submit: function (e) {
      //ifFdata (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = _.extend({
        language: $("#language").val(),
        enjoyment: $("#enjoyment").val(),
        asses: $('input[name="assess"]:checked').val(),
        age: $("#age").val(),
        gender: $("#gender").val(),
        education: $("#education").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val(),
        comments: $("#comments").val()
      });
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name: "thanks",
    start: function () {
      exp.data = {
        "trials": exp.data_trials,
        "catch_trials": exp.catch_trials,
        "system": exp.system,
        "condition": exp.condition,
        "subject_information": exp.subj_data,
        "time_in_minutes": (Date.now() - exp.startT) / 60000
      };
      setTimeout(function () { proliferate.submit(exp.data); }, 1000);

      

    }
  });

  return slides;
}

/// init ///
function init() {

  repeatWorker = false;


  //exp.n_entities = 1;
  exp.names = [];
  exp.all_names = [];
  exp.trials = [];
  exp.catch_trials = [];
  var stimuli = generate_stim(); // this calls a function in stimuli.js

  console.log(stimuli.length);
  //exp.stimuli = _.shuffle(stimuli).slice(0, 15);
  exp.stimuli = stimuli.slice();
  exp.stimuli = _.shuffle(exp.stimuli);
  exp.n_trials = exp.stimuli.length;
  exp.stimcounter = 0;

  exp.stimscopy = exp.stimuli.slice();

  exp.system = {
    Browser: BrowserDetect.browser,
    OS: BrowserDetect.OS,
    screenH: screen.height,
    screenUH: exp.height,
    screenW: screen.width,
    screenUW: exp.width
  };
  //blocks of the experiment:
  exp.structure = [
    // "bot",
    // "i0",
    "example1",
    "example2",
    "startExp",
    "generateEntities", // This is where the test trials come in.
    //"priors",
    "subj_info",
    "thanks"
  ];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
  //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function () {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function () { $("#mustaccept").show(); });
      exp.go();
    }
  });

  exp.go(); //show first slide
}

