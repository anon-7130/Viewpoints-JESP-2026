window.addEventListener("keydown", function (event) {
	if (event.key !== undefined) {
		if(event.key === "s") {
			document.getElementById("pressKey").remove()
			document.getElementById("mainHidden").style.visibility = "visible";
		}
}});