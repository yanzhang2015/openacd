/* Load this script using conditional IE comments if you need to support IE 7 and IE 6. */

window.onload = function() {
	function addIcon(el, entity) {
		var html = el.innerHTML;
		el.innerHTML = '<span style="font-family: \'icomoon\'">' + entity + '</span>' + html;
	}
	var icons = {
			'icon-pushpin' : '&#xe601',
			'icon-calendar' : '&#xe600',
			'icon-group-chat' : '&#xe035;',
			'icon-presence_on_the_phone' : '&#xe090;',
			'icon-alarm' : '&#xe034;',
			'icon-presence_busy' : '&#xe08b;',
			'icon-presence_meeting' : '&#xe08c;',
			'icon-presence_dnd' : '&#xe08f;',
			'icon-call_incoming' : '&#xe001;',
			'icon-call_outgoing' : '&#xe000;',
			'icon-close' : '&#xe002;',
			'icon-ezuce' : '&#xe003;',
			'icon-busy' : '&#xe004;',
			'icon-calls_outgoing' : '&#xe005;',
			'icon-call_failed' : '&#xe006;',
			'icon-call_history' : '&#xe007;',
			'icon-emoticons' : '&#xe008;',
			'icon-contacts' : '&#xe009;',
			'icon-chat_to_call' : '&#xe00c;',
			'icon-chat' : '&#xe00d;',
			'icon-call_back' : '&#xe024;',
			'icon-stop' : '&#xe067;',
			'icon-rewind' : '&#xe07b;',
			'icon-voicemail' : '&#xe05f;',
			'icon-send_voicemail' : '&#xe074;',
			'icon-forward' : '&#xe052;',
			'icon-hold' : '&#xe04b;',
			'icon-logout' : '&#xe045;',
			'icon-fast_rewind' : '&#xe057;',
			'icon-fast_forward' : '&#xe058;',
			'icon-volume_speaker_2' : '&#xe05a;',
			'icon-settings_cog' : '&#xe073;',
			'icon-volume_mic' : '&#xe05d;',
			'icon-mail' : '&#xe044;',
			'icon-location' : '&#xe046;',
			'icon-volume_speaker_1_mute' : '&#xe05c;',
			'icon-search_profile' : '&#xe077;',
			'icon-state_write' : '&#xe06a;',
			'icon-view_profile' : '&#xe00f;',
			'icon-idle' : '&#xe011;',
			'icon-phone_mobile' : '&#xe039;',
			'icon-incoming' : '&#xe013;',
			'icon-minimize' : '&#xe014;',
			'icon-outgoing' : '&#xe015;',
			'icon-bubbles' : '&#xe017;',
			'icon-address' : '&#xe016;',
			'icon-transfer-arrow' : '&#xe019;',
			'icon-redial' : '&#xe01a;',
			'icon-transfer_call' : '&#xe064;',
			'icon-notes' : '&#xe03d;',
			'icon-missed_calls' : '&#xe043;',
			'icon-phone_office' : '&#xe038;',
			'icon-phone_fax' : '&#xe03a;',
			'icon-external_link' : '&#xe01b;',
			'icon-chat_history' : '&#xe025;',
			'icon-arrow-left' : '&#xe01c;',
			'icon-arrow-up' : '&#xe01d;',
			'icon-arrow-right' : '&#xe01e;',
			'icon-arrow-down' : '&#xe01f;',
			'icon-trash' : '&#xe063;',
			'icon-connect' : '&#xe020;',
			'icon-alert_2' : '&#xe021;',
			'icon-search' : '&#xe081;',
			'icon-send_file' : '&#xe075;',
			'icon-user' : '&#xe00b;',
			'icon-users' : '&#xe00a;',
			'icon-transfer-arrow-with-border' : '&#xe018;',
			'icon-call_ingoing' : '&#xe023;',
			'icon-precall' : '&#xe022;',
			'icon-plus' : '&#xe00e;',
			'icon-add_to_conference' : '&#xe028;',
			'icon-conference_call' : '&#xe026;',
			'icon-record' : '&#xe07e;',
			'icon-volume_mic_mute' : '&#xe05e;',
			'icon-camera' : '&#xe027;',
			'icon-dialpad' : '&#xe029;',
			'icon-connect_to_1' : '&#xe02a;',
			'icon-add_to_group_chat' : '&#xe02b;',
			'icon-slate_wait' : '&#xe06f;',
			'icon-help' : '&#xe04d;',
			'icon-icon_volume_speaker_1' : '&#xe082;',
			'icon-address_book' : '&#xe083;',
			'icon-wrap' : '&#xe010;',
			'icon-download' : '&#xe02f;',
			'icon-gadget-remove' : '&#xe02d;',
			'icon-gadget-close' : '&#xe02e;',
			'icon-gadget-settings' : '&#xe030;',
			'icon-gadget-open' : '&#xe031;',
			'icon-play' : '&#xe02c;',
			'icon-play-long' : '&#xe032;',
			'icon-download-folder' : '&#xe033;'
		},
		els = document.getElementsByTagName('*'),
		i, attr, c, el;
	for (i = 0; ; i += 1) {
		el = els[i];
		if(!el) {
			break;
		}
		attr = el.getAttribute('data-icon');
		if (attr) {
			addIcon(el, attr);
		}
		c = el.className;
		c = c.match(/icon-[^\s'"]+/);
		if (c && icons[c[0]]) {
			addIcon(el, icons[c[0]]);
		}
	}
};