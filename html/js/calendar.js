/* jshint esversion: 8 */
document.addEventListener("DOMContentLoaded", async function() {
    const target = document.getElementsByClassName('calendar')[0];

    const today = new Date();
    let offset = 0;
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
    heading_el.textContent = `Týden ${title}`;

    target.appendChild(heading_el);

    const romcal = new Romcal({
        localizedCalendar: CzechRepublic_Cs,
        scope: 'gregorian',
    });

    let calendars = {};

    for (let day of week) {
        const year = day.getFullYear();
        if (!(year in calendars)) {
            calendars[year] = await romcal.generateCalendar(year);
        }
        const calendar = calendars[year];

        const day_key = day.toISOString().substring(0, 10);
        const lit_day = calendar[day_key].filter(d => !d.name.includes('$')); // filter out days with wrong names

        const day_el = document.createElement('p');
        day_el.textContent = lit_day.map(d => d.name).join(', ');

        target.appendChild(day_el);
    }
});
