/* jshint esversion: 8 */
document.addEventListener("DOMContentLoaded", function() {
    let body = document.body;
    function pslm_curr_size() {
        for (let i = 0; i < pslm_svg_sizes.length; ++i) {
            let el = document.getElementsByClassName('size-' + pslm_svg_sizes[i])[0];
            let style = getComputedStyle(el);
            if (style.display == 'inline-block') {
                return i;
            }
        }
        return false;
    }
    function pslm_curr_zoom() {
        for (let cl of body.classList) {
            if (cl.substring(0, 4) == 'zoom') {
                return parseInt(cl.substring(5));
            }
        }
        return false;
    }
    function pslm_set_zoom(zoom) {
        for (let cl of body.classList) {
            if (cl.substring(0, 4) == 'zoom') {
                body.classList.remove(cl);
            }
        }
        localStorage.setItem('pslm_zoom', zoom);
        body.classList.add('zoom-' + zoom);
    }
    function pslm_zoom_in(e) {
        let size = pslm_curr_size();
        let new_size = size;
        let zoom = pslm_curr_zoom();
        let old_zoom = zoom;
        let max_zoom = pslm_svg_sizes.length - 1;

        while (zoom < max_zoom && new_size == size) {
            pslm_set_zoom(++zoom);
            new_size = pslm_curr_size();
        }
        if (new_size == size) {
            // zooming was impossible
            pslm_set_zoom(old_zoom);
        }
        e.preventDefault();
        return false;
    }
    function pslm_zoom_out(e) {
        let size = pslm_curr_size();
        let new_size = size;
        let zoom = pslm_curr_zoom();
        let old_zoom = zoom;
        let min_zoom = -(pslm_svg_sizes.length - 1);

        while (zoom > min_zoom && new_size == size) {
            pslm_set_zoom(--zoom);
            new_size = pslm_curr_size();
        }
        if (new_size == size) {
            // zooming was impossible
            pslm_set_zoom(old_zoom);
        }
        e.preventDefault();
        return false;
    }
    function pslm_zoom_reset(e) {
        pslm_set_zoom(0);
        e.preventDefault();
        return false;
    }
    document.getElementById("zoom-in-button").addEventListener("click", pslm_zoom_in);
    document.getElementById("zoom-out-button").addEventListener("click", pslm_zoom_out);
    document.getElementById("zoom-reset-button").addEventListener("click", pslm_zoom_reset);

    let stored_zoom = localStorage.getItem('pslm_zoom');
    if (stored_zoom !== null) {
        pslm_set_zoom(stored_zoom);
    }
});
