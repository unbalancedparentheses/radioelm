var audio5js;

var radio = Elm.fullscreen(Elm.Radio);

radio.ports.name.subscribe(function (state){

    console.log(state);

    if (audio5js && typeof audio5js.destroy == 'function') {
        audio5js.destroy();
    }

    if (state.playing === true) {

        audio5js = new Audio5js({
            swf_path: './audio5js.swf',
            ready: function () {
                this.load(state.radio.url);
                this.play();
            }
        });
    }

});
