/* jshint esversion: 8 */
document.addEventListener("DOMContentLoaded", async function() {
    const week_target = document.querySelector('.calendar .week');
    const today_target = document.querySelector('.calendar .today');
    const sunday_target = document.querySelector('.calendar .sunday');
    let offset = 0;

    let last_render_date = undefined;

    window.setInterval(() => {
        const last_render_date_key = date_to_key(last_render_date);
        const now_key = date_to_key(new Date());
        if (last_render_date_key !== now_key) {
            // rerender
            console.log('date changed, rerender calendar with default offset');
            offset = 0;
            render_calendar(offset);
        }
    }, 2000);
    
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

    const ranks = {
        FEAST: 'Svátek',
        MEMORIAL: 'Památka',
        SOLEMNITY: 'Slavnost',
    };

    await render_calendar(offset);

    function date_to_key(day) {
        const year_string = day.getFullYear().toString();
        const month_string = (day.getMonth()+1).toString().padStart(2, "0");
        const day_string = day.getDate().toString().padStart(2, "0");
        const day_key =  `${year_string}-${month_string}-${day_string}`;
        return day_key;
    }

    async function render_calendar(offset) {
        week_target.innerHTML = '';

        let today_div;
        let sunday_div;

        const today = new Date();
        last_render_date = today;
        let week = [];

        for (let i = 0; i <= 8; ++i) {
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
        const week_heading_el = document.createElement('h2');
        week_heading_el.innerText = `Týden ${title}`;
        week_target.appendChild(week_heading_el);

        const button_el = document.createElement('p');
        button_el.innerHTML = ' <a href="#" class="prev-week-button">Předchozí</a> – <a href="#" class="next-week-button">Následující</a>';
        button_el.classList.add('calendar-buttons');
        week_target.appendChild(button_el);

        async function get_day_psalms_and_lit_days(i, day, day_psalms, lit_days) {
            const year = day.getFullYear();
            if (!(year in calendars)) {
                calendars[year] = await romcal.generateCalendar(year);
            }
            const calendar = calendars[year];
            const day_key = date_to_key(day);
            const lit_events = calendar[day_key].filter(d => !d.name.includes('$')); // filter out days with wrong names
            const lit_event_keys = lit_events.map(e => e.key);

            // include also weekday if missing
            for (const event of lit_events) {
                if (event.rank !== 'FEAST' && event.weekday && !lit_event_keys.includes(event.weekday.key)) {
                    lit_events.push(event.weekday);
                }
            }

            function add_psalms(rank, key, i) {
                if (key in pslm_romcal_to_occasions) {
                    for (const occasion of pslm_romcal_to_occasions[key]) {
                        if (occasion.match(/vigilie/i)) {
                            if (i > 0) {
                                day_psalms[i-1].push({ rank: lit_days[i-1].length, occasion, psalms: pslm_psalms[occasion] });
                            }
                        } else {
                            day_psalms[i].push({ rank, occasion, psalms: pslm_psalms[occasion] });
                        }
                    }
                }
            }
            day_psalms[i] = [];
            add_psalms(0, `${day.getDate()}/${day.getMonth()+1}`, i);

            for (const [rank, event] of lit_events.entries()) {
                const key = event.key;
                add_psalms(rank, key, i);
                const sunday_cycle = event.cycles.sundayCycle.slice(-1);
                add_psalms(rank, `${key}|${sunday_cycle}`, i);
                const weekday_cycle = event.cycles.weekdayCycle.slice(-1);
                add_psalms(rank, `${key}|${weekday_cycle}`, i);
            }
            lit_days[i] = lit_events;
        }

        let lit_days = [];
        let day_psalms = [];

        for (const [i, day] of week.entries()) {
            await get_day_psalms_and_lit_days(i, day, day_psalms, lit_days);
        }
        // delete the extra last day added for finding vigils
        day_psalms = day_psalms.slice(0, -1);
        
        for (let [i, psalms] of day_psalms.entries()) {
            
            psalms.sort(function(a, b) {
                if (a.rank < b.rank) {
                    return -1;
                } else if (a.rank > b.rank) {
                    return 1;
                } else {
                    if (a.occasion < b.occasion) {
                        return -1;
                    } else if (a.occasion > b.occasion) {
                        return 1;
                    } else {
                        return 0;
                    }
                }
            });
            const day_div = document.createElement('div');
            day_div.classList.add('day');

            const day_el = document.createElement('p');
            day_el.classList.add('day-title');

            if (week[i] === today) {
                day_el.classList.add('today');
                today_div = day_div;
            } else if (i === day_psalms.length - 1) {
                sunday_div = day_div;
            }
            let day_title = lit_days[i].map(d => {
                let title;
                if (ranks[d.rank]) {
                    title = ranks[d.rank] + ' ' + d.name;
                } else {
                    title = d.name;
                }
                return title.replace('Sv.', 'sv.').replace('Bl.', 'bl.');
            }).join(', ');
            if (week[i].getDay() === 0) {
                day_title = `<strong>${day_title}</strong>`;
            }
            day_el.innerHTML = `<strong>${week[i].getDate()}. ${week[i].getMonth()+1}.</strong> – ${day_title}</strong>`;
            day_div.appendChild(day_el);

            const list_el = document.createElement('ul');
            let list = [];
            const show_occasion = psalms.length > 1;
            for (const p of psalms) {
                const occasion = p.occasion.replace(/ \([^\)]+\)/, '');
                const occasion_html = show_occasion ? `<br /><span>(${occasion})</span>` : '';
                for (const id of p.psalms) {
                    list.push(`<li><a href="${id}.html">${pslm_titles[id]}</a>${occasion_html}</li>`);
                }
            }
            list_el.innerHTML = list.join('');
            day_div.appendChild(list_el);
            week_target.appendChild(day_div);
        }

        if (today_div !== undefined) {
            today_target.innerHTML = '';
            sunday_target.innerHTML = '';

            const today_heading_el = document.createElement('h2');
            today_heading_el.innerText = 'Dnes';
            today_target.appendChild(today_heading_el);

            const sunday_heading_el = document.createElement('h2');
            sunday_heading_el.innerText = 'Nadcházející neděle';
            sunday_target.appendChild(sunday_heading_el);

            today_target.appendChild(today_div.cloneNode(true));
            sunday_target.appendChild(sunday_div.cloneNode(true));
        }

        // print days with no psalm in the whole year
        /*
        const day = new Date();
        day.setFullYear(2022);
        day.setMonth(0);
        day.setDate(1);
        const current_year = day.getFullYear();

        lit_days = [];
        day_psalms = [];
        all_days = [];
        let i = 0;

        while (day.getFullYear() === current_year) {
            await get_day_psalms_and_lit_days(i, day, day_psalms, lit_days);
            all_days[i] = new Date(day);
            day.setDate(day.getDate() + 1);
            ++i;
        }
        for (const [i, day] of all_days.entries()) {
            if (day_psalms[i].length === 0) {
                console.log(day);
                console.log(lit_days[i]);
            }
        }
        */
    }
});
