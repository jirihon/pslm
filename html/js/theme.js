/* jshint esversion: 8 */
// @ts-check
document.addEventListener("DOMContentLoaded", function() {
    /**
     * @typedef {'light' | 'dark' | 'os-default'} Theme
     */

    const storage_key = 'theme-preference';

    /**
     * @returns {Theme}
     */
    function get_theme_preference() {
        let theme = localStorage.getItem(storage_key);
        if (theme === 'light') {
            return 'light';
        } else if (theme === 'dark') {
            return 'dark';
        } else {
            return 'os-default';
        }
    }

    /**
     * @param {Theme} theme
     * @returns {void}
     */
    function set_theme(theme) {
        document.firstElementChild?.setAttribute('class', theme);
        
        if (theme !== 'os-default') {
            localStorage.setItem(storage_key, theme);
        } else {
            localStorage.removeItem(storage_key);
        }
    }
    document.getElementById("light-theme")?.addEventListener("click", (e) => { set_theme('light'); e.preventDefault(); });
    document.getElementById("dark-theme")?.addEventListener("click", (e) => { set_theme('dark'); e.preventDefault(); });
    document.getElementById("os-default-theme")?.addEventListener("click", (e) => { set_theme('os-default'); e.preventDefault(); });

    document.firstElementChild?.setAttribute('class', get_theme_preference());
});