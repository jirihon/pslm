/* jshint esversion: 8 */
document.addEventListener("DOMContentLoaded", function () {
    const search_field = document.getElementsByClassName('search-field')[0];
    search_field.addEventListener('input', search);
    const target = document.getElementsByClassName('search')[0];

    function render_hits(hits) {
        target.innerHTML = '';

        for (const hit of hits) {
            const p_el = document.createElement('p');
            p_el.innerText = hit;
            target.appendChild(p_el);

            const list_el = document.createElement('ul');
            let list = [];
            for (const id of pslm_psalms[hit]) {
                list.push(`<li><a href="${id}.html">${pslm_titles[id]}</a></li>`)
            }
            list_el.innerHTML = list.join('');
            target.appendChild(list_el);
        }
    }

    function normalize_string(string) {
        return removeDiacritics(string.toLowerCase().replaceAll(',', ''));
    }

    function search(e) {
        const needle = normalize_string(e.target.value).replaceAll(/\s+/g, ' ').trim();
        let hits = [];
        if (needle.length === 0) {
            render_hits(hits);
            return;
        }
        let n = 0;
        for (const key of Object.keys(pslm_psalms)) {
            const haystack = normalize_string(key);
            if (haystack.includes(needle)) {
                hits.push(key);
                ++n;
            }
            if (n > 20) {
                break;
            }
        }
        render_hits(hits);
    }
});
