const fs = require('fs');

const data = JSON.parse(fs.readFileSync("./map.geo.json"));
let out = {};

for(const feature of data.features) {
    const country_name = feature.properties.admin;
    const country_code = feature.properties.iso_a2_eh;

    if(country_code == -99) {
        console.error("skipping", country_name);
        continue;
    }

    let geometry;
    if(feature.geometry.type === "Polygon") {
        geometry = [feature.geometry.coordinates];
    } else {
        geometry = feature.geometry.coordinates;
    }

    out[country_code] = {
        country_name,
        geometry
    }
}

console.log(JSON.stringify(out));
