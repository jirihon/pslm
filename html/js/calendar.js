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
            const day_offset = i - today.getDay() + offset*7;
            if (day_offset == 0) {
                week.push(today);
            } else {
                let day = new Date(today.getTime());
                day.setDate(today.getDate() + day_offset);
                week.push(day);
            }
        }
        const s = week[0];
        const e = week[7];
        let title;
        
        if (s.getFullYear() == e.getFullYear()) {
            if (s.getMonth() == e.getMonth()) {
                title = `${s.getDate()}.–${e.getDate()}. ${e.getMonth()+1}. ${e.getFullYear()}`;
            } else {
                title = `${s.getDate()}. ${s.getMonth()+1}. – ${e.getDate()}. ${e.getMonth()+1}. ${e.getFullYear()}`;
            }
        } else {
            title = `${s.getDate()}. ${s.getMonth()+1}. ${s.getFullYear()} – ${e.getDate()}. ${e.getMonth()+1}. ${e.getFullYear()}`;
        }
        const heading_el = document.createElement('h2');
        heading_el.innerText = `Týden ${title}`;

        target.appendChild(heading_el);

        const button_el = document.createElement('p');
        button_el.innerHTML = ' <a href="#" class="prev-week-button">Předcházející</a> – <a href="#" class="next-week-button">Následující</a>';
        button_el.classList.add('calendar-buttons');
        target.appendChild(button_el);

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
            let lit_events = calendar[day_key].filter(d => !d.name.includes('$')); // filter out days with wrong names
            
            // include also weekday if missing
            for (const event of lit_events) {
                if (event.weekday && !lit_events.map(e => e.key).includes(event.weekday.key)) {
                    lit_events.push(event.weekday);
                }
            }
            for (const event of lit_events) {
                const key = event.key;
                add_psalms(key, i);
                const sunday_cycle = event.cycles.sundayCycle.substr(5);
                add_psalms(`${key}|${sunday_cycle}`, i);
                const weekday_cycle = event.cycles.weekdayCycle.substr(5);
                add_psalms(`${key}|${weekday_cycle}`, i);
                add_psalms(`${day.getDate()}/${day.getMonth()+1}`, i);
            }
            lit_days[i] = lit_events;
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
            day_el.classList.add('calendar-day');
            if (week[i] === today) {
                day_el.classList.add('calendar-current-day');
            }
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
