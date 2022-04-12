/* jshint esversion: 8 */
document.addEventListener("DOMContentLoaded", async function() {
    const target = document.getElementsByClassName('calendar')[0];
    let offset = 0;
    
    const romcal = new Romcal({
        localizedCalendar: CzechRepublic_Cs,
        scope: 'gregorian',
    });

    let calendars = {};

    document.addEventListener('click', function(e) {
        if (e.target.closest('.prev-week-button')) {
            offset -= 1;
            render_calendar(offset);
            e.preventDefault();
            return false;
        } else if (e.target.closest('.next-week-button')) {
            offset += 1;
            render_calendar(offset);
            e.preventDefault();
            return false;
        }
    });

    render_calendar(offset);

    async function render_calendar(offset) {
        target.innerHTML = '';

        const today = new Date();
        let week = [];

        for (let i = 0; i <= 7; ++i) {
            let day = new Date(today.getTime());
            day.setDate(today.getDate() + (i - today.getDay()) + offset*7);
            week.push(day);
        }
        const s = week[0];
        const e = week[7];
        let title;
        
        if (s.getFullYear() == e.getFullYear()) {
            title = `${s.getDate()}. ${s.getMonth()+1}. – ${e.getDate()}. ${e.getMonth()+1}. ${e.getFullYear()}`;
        } else {
            title = `${s.getDate()}. ${s.getMonth()+1}. ${s.getFullYear()} – ${e.getDate()}. ${e.getMonth()+1}. ${e.getFullYear()}`;
        }
        const heading_el = document.createElement('h2');
        heading_el.innerHTML = `Týden ${title} <a href="#" class="prev-week-button"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M224 480c-8.188 0-16.38-3.125-22.62-9.375l-192-192c-12.5-12.5-12.5-32.75 0-45.25l192-192c12.5-12.5 32.75-12.5 45.25 0s12.5 32.75 0 45.25L77.25 256l169.4 169.4c12.5 12.5 12.5 32.75 0 45.25C240.4 476.9 232.2 480 224 480z"/></svg></a> <a href="#" class="next-week-button"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M96 480c-8.188 0-16.38-3.125-22.62-9.375c-12.5-12.5-12.5-32.75 0-45.25L242.8 256L73.38 86.63c-12.5-12.5-12.5-32.75 0-45.25s32.75-12.5 45.25 0l192 192c12.5 12.5 12.5 32.75 0 45.25l-192 192C112.4 476.9 104.2 480 96 480z"/></svg></a>`;

        target.appendChild(heading_el);

        let lit_days = [];
        let day_psalms = [];

        function add_psalms(key, i) {
            if (key in pslm_romcal_to_occasions) {
                for (const occasion of pslm_romcal_to_occasions[key]) {
                    const value = { occasion, psalms: pslm_psalms[occasion] };
                    if (occasion.match(/vigilie/i)) {
                        if (i > 0) {
                            day_psalms[i-1].push(value);
                        }
                    } else {
                        day_psalms[i].push(value);
                    }
                }
            }
        }

        for (const [i, day] of week.entries()) {
            day_psalms[i] = [];
            const year = day.getFullYear();
            if (!(year in calendars)) {
                calendars[year] = await romcal.generateCalendar(year);
            }
            const calendar = calendars[year];
            const day_key = day.toISOString().substring(0, 10);
            const lit_day = calendar[day_key].filter(d => !d.name.includes('$')); // filter out days with wrong names
            lit_days[i] = lit_day;

            for (const record of lit_day) {
                const key = record.key;
                add_psalms(key, i);
                const sunday_cycle = record.cycles.sundayCycle.substr(5);
                add_psalms(`${key}|${sunday_cycle}`, i);
                const weekday_cycle = record.cycles.weekdayCycle.substr(5);
                add_psalms(`${key}|${weekday_cycle}`, i);
            }
        }
        
        for (let [i, psalms] of day_psalms.entries()) {
            
            psalms.sort(function(a, b) {
                if (a.occasion < b.occasion) {
                    return -1;
                } else if (a.occasion > b.occasion) {
                    return 1;
                } else {
                    return 0;
                }
            });
            const ids = psalms.map(p => p.psalms).flat();

            const day_el = document.createElement('p');
            let day_title = lit_days[i].map(d => d.name).join(', ');
            if (week[i].getDay() === 0) {
                day_title = `<strong>${day_title}</strong>`;
            }
            day_el.innerHTML = `<strong>${week[i].getDate()}. ${week[i].getMonth()+1}.</strong> – ${day_title}</strong>`;
            target.appendChild(day_el);

            const list_el = document.createElement('ul');
            let list = [];
            for (const id of ids) {
                list.push(`<li><a href="${id}.html">${pslm_titles[id]}</a></li>`)
            }
            list_el.innerHTML = list.join('');
            target.appendChild(list_el);
        }
    }
});
