--[[
	Version 2.0.0a
	This is intended for Roblox ModuleScripts.
	It works on vanilla Lua, but there are far superior implementations you should use instead.
	BSD 2-Clause Licence
	Copyright ©, 2020 - Blockzez (devforum.roblox.com/u/Blockzez and github.com/Blockzez)
	All rights reserved.
	
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:
	
	1. Redistributions of source code must retain the above copyright notice, this
	   list of conditions and the following disclaimer.
	
	2. Redistributions in binary form must reproduce the above copyright notice,
	   this list of conditions and the following disclaimer in the documentation
	   and/or other materials provided with the distribution.
	
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
	FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
]]--
-- International (not International Core) Module Support, just place the path of the module here (don't run require).
local intl;

local divmod = function(left, right) return math.floor(left / right), math.floor(left % right) end;
local dt = { };
local dt_proxy = { };
--[=[ DateTime & TimeSpan formatter ]=]--

--[=[
	For DateTime, see CLDR https://unicode.org/reports/tr35/tr35-dates.html#Date_Format_Patterns
	Only the following pattern are valid:
	y, M, d, a, h, H, J, j, m, s, Z, X, x, S
	B and b aren't supported because day period data aren't supported.
	Z are supported but z, v and V aren't because it's impossible to determine timezone without location
	Attempting to input everything else will throw an error. If you want the literal character use "'"
	If you're looking for fully fledged feature, see International and International Core.
	For TimeSpan:
		d - Day
		h - Hour
		m - Minute
		s - Second
		S (or f) - Millisecond
		' - Literal ('' for literal ')
]=]--
local month_names = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
local quarter_names = { 'Q1', 'Q2', 'Q3', 'Q4' };
local day_names = { "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" };
local full_month_names = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };
local full_day_names = { "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" };
local full_quarter_names = { "1st quarter", "2nd quarter", "3rd quarter", "4th quarter" };

local function offset_to_string(offset, sep, include_sign)
	local o_h, o_m = divmod(offset, 60);
	if o_h == 0 and o_m == 0 and sep:match('^iso') then
		return 'Z';
	end;
	return (o_h >= 0 and "+%02d%s%02d" or "%02d%s%02d"):format(o_h, sep:match('^iso') and sep:sub(4) or sep, o_m);
end;

local formats = { };
local function zero_pad(value, n)
	return ('%0' .. n .. 'd'):format(value);
end;
formats.d = 
	setmetatable({
		y = function(self, data, utc, n)
			local yr = (utc and self.UtcYear or self.Year);
			if n == 2 then
				yr = yr % 100;
			end;
			return zero_pad(yr, n);
		end;
		Q = function(self, data, utc, n)
			local q = math.ceil((utc and self.UtcMonth or self.Month) / 3);
			if n < 3 then
				return zero_pad(q, n);
			end;
			return (n == 4 and data.WideQuarterNames or data.QuarterNames)[q];
		end;
		M = function(self, data, utc, n)
			local mo = (utc and self.UtcMonth or self.Month);
			if n < 3 then
				return zero_pad(mo, n);
			end;
			return (n == 4 and data.WideMonthNames or data.MonthNames)[mo];
		end;
		d = function(self, data, utc, n)
			local mo = (utc and self.UtcDay or self.Day);
			return zero_pad(mo, n);
		end;
		E = function(self, data, utc, n)
			return (n == 4 and data.WideDayNames or data.DayNames)[utc and self.UtcDayOfWeek or self.DayOfWeek];
		end;
		e = function(self, data, utc, n)
			local e = (utc and self.UtcDayOfWeek or self.DayOfWeek);
			if n < 3 then
				return zero_pad(e, n);
			end;
			return (n == 4 and data.WideDayNames or data.DayNames)[e];
		end;
	},
	{
		__index = function(self, index)
			error("Format inputted was not in a correct format", 3);
		end;
	}
);
formats.dt = 
	setmetatable({
		a = function(self, data, utc, n)
			local hr = (utc and self.UtcHour or self.Hour);
			return zero_pad(hr < 12 and data.AMName or data.PMName, n);
		end;

		h = function(self, data, utc, n)
			return zero_pad((((utc and self.UtcHour or self.Hour) - 1) % 12) + 1, n);
		end;
		H = function(self, data, utc, n)
			return zero_pad(utc and self.UtcHour or self.Hour, n);
		end;
		K = function(self, data, utc, n)
			return zero_pad(((utc and self.UtcHour or self.Hour) % 12), n);
		end;
		k = function(self, data, utc, n)
			return zero_pad((((utc and self.UtcHour or self.Hour) - 1) % 24) + 1, n);
		end;
		m = function(self, data, utc, n)
			return zero_pad(utc and self.UtcMinute or self.Minute, n);
		end;
		s = function(self, data, utc, n)
			return zero_pad(utc and self.UtcSecond or self.Second, n);
		end;
		S = function(self, data, utc, n)
			return zero_pad(utc and self.UtcMillisecond or self.Millisecond, n);
		end;
		f = function(self, data, utc, n)
			return zero_pad(utc and self.UtcMillisecond or self.Millisecond, n);
		end;
		Z = function(self, data, utc, n)
			local offset = self.TimezoneOffset;
			return (n == 4 and 'GMT' or '') .. offset_to_string(offset, n < 3 and '' or n == 4 and ':' or 'iso:', true);
		end;
		O = function(self, data, utc, n)
			local offset = self.TimezoneOffset;
			return 'GMT' .. (n == 1 and offset_to_string(offset, ':', true) or offset_to_string(offset, ':', true):gsub('^([+-])0', '%1'):gsub(':00$', ''));
		end;
		X = function(self, data, utc, n)
			local offset = self.TimezoneOffset;
			return (n == 1 and offset_to_string(offset, 'iso' .. ((n == 3 or n == 5) and ':' or ''), true):gsub('00$', ''));
		end;
		x = function(self, data, utc, n)
			local offset = self.TimezoneOffset;
			return (n == 1 and offset_to_string(offset, ((n == 3 or n == 5) and ':' or ''), true):gsub('00$', ''));
		end;
	},
	{
		__index = formats.d;
	}
);
formats.ts = 
	setmetatable({
		h = function(self, data, n)
			return zero_pad(data.ModHours and (self.Hours % 24) or self.Hours, n);
		end;
		m = function(self, data, n)
			return zero_pad(data.ModMinutes and (self.Minutes % 24) or self.Minutes, n);
		end;
		s = function(self, data, n)
			return zero_pad(data.ModSeconds and (self.Seconds % 24) or self.Seconds, n);
		end;
		f = function(self, data, n)
			return zero_pad(data.ModMilliseconds and (self.Milliseconds % 24) or self.Milliseconds, n);
		end;
		S = function(self, data, n)
			return zero_pad(data.ModMilliseconds and (self.Milliseconds % 24) or self.Milliseconds, n);
		end;
	},
	{
		__index = function(self, index)
			error("Format inputted was not in a correct format", 3);
		end;
	}
);

local max_char =
{
	G = 5,
	Q = 5, q = 5,
	M = 5, L = 5,
	w = 2, W = 1,
	d = 2, D = 3, F = 1,
	E = 6, e = 6, c = { true, false, true, true, true, true },
	a = 1,
	h = 2, H = 2, K = 2, k = 2,
	m = 2,
	s = 2,
	z = 4, Z = 5, O = { true, false, false, true }, v = { true, false, false, true },
	V = 4, x = 5, X = 5
};
local pattern_char_order = "GyYuUQqMLlwWdDFgEecabBChHKkjJmsSAzZOvVXx";
local ts_pattern_char_order = "dhmsfS";
local function format(t, data, utc, self, ptn)
	if data == nil then
		data =
			utc == nil and {
				ModHours = true;
				ModMinutes = true;
				ModSeconds = true;
				ModMilliseconds = true;
			} or {
				QuarterNames = quarter_names;
				WideQuarterNames = full_quarter_names;
				MonthNames = month_names;
				WideMonthNames = full_month_names;
				DayNames = day_names;
				WideDayNames = full_day_names;
				AMName = 'AM';
				PMName = 'PM';
			};
	elseif type(data) ~= "table" then
		error("Data inputted must be a table", 2);
	else
		data =
			utc == nil and {
				ModHours = data.ModHours ~= false;
				ModMinutes = data.ModMinutes ~= false;
				ModSeconds = data.ModSeconds ~= false;
				ModMilliseconds = data.ModMilliseconds ~= false;
			} or {
				QuarterNames = data.QuarterNames or quarter_names;
				WideQuarterNames = data.WideQuarterNames or full_quarter_names;
				MonthNames = data.MonthNames or month_names;
				WideMonthNames = data.WideMonthNames or full_month_names;
				DayNames = data.DayNames or day_names;
				WideDayNames = data.WideDayNames or full_day_names;
				AMName = data.AMName or 'AM';
				PMName = data.PMName or 'PM';
			};
	end;
	local ret = '';
	local i0 = 0;
	while i0 do
		local i1 = ptn:find(utc == nil and ('[' .. ts_pattern_char_order .. '%%]') or ('[' .. pattern_char_order .. "']"), i0);
		if i1 then
			if ptn:sub(i0, i1 - 1) ~= '' then
				ret = ret .. ptn:sub(i0, i1 - 1);
			end;
			local chr = ptn:sub(i1, i1);
			if chr == "'" then
				i0 = ptn:find("'", i1 + 1);
				if not i0 then
					error("Format inputted was not in a correct format", 2);
				end;
				if i0 == i1 + 1 then
					ret = ret .. "'";
				else
					ret = ret .. ptn:sub(i1 + 1, i0 - 1);
				end;
				i0 = i0 + 1;
			else
				local i2 = ((utc == nil and chr == '%') and ptn:find('[' .. ts_pattern_char_order .. ']') or ptn:find('[^' .. chr .. ']+', i1 + 1)) or (#ptn + 1);
				
				if type(max_char[chr]) == "table" then
					if not max_char[chr][i2 - i1] then
						error("Format inputted was not in a correct format", 2);
					end;
				elseif not i2 then
					error("Format inputted was not in a correct format", 2);
				else
					if (i2 - i1) > (max_char[chr] or math.huge) then
						error("Format inputted was not in a correct format", 2);
					end;
				end;
				
				ret = ret .. utc == nil and t[chr](self, data, i2 - i1) or t[chr](self, data, utc, i2 - i1);
				i0 = i2;
			end;
		else
			if ptn:sub(i0) ~= '' then
				ret = ret .. ptn:sub(i0);
			end;
			i0 = nil;
		end;
	end;
	return ret;
end;

--[=[ Private functions ]=]--
local min_val = -8640000000000000;
local max_val = 8640000000000000;
local month_in_days = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
local months_to_days = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };

local function is_leap(y)
	return (y % 4 == 0) and ((y % 100 ~= 0) or (y % 400 == 0));
end;

local function from_date(y, m, d, md)
	local mdr = (months_to_days[m] + ((m > 2 and is_leap(y)) and 1 or 0)) + (d - 1);
	if md then
		return mdr;
	end;
	y = y - 1;
	return (y * 365) + math.floor(y / 4) - math.floor(y / 100) + math.floor(y / 400) + mdr;
end;

local days_in_year = 365;
local days_in_4_years = from_date(5, 1, 1); -- Leap, 1'461
local days_in_100_years = from_date(101, 1, 1); -- Not leap, 36'524
local days_in_400_years = from_date(401, 1, 1); -- Leap, 146'097
local days_in_1970_years = from_date(1970, 1, 1);

local function to_date(n)
	local y400, y100, y4, y;
	y400, n = divmod(n, days_in_400_years);
	y100, n = divmod(n, days_in_100_years);
	y4, n = divmod(n, days_in_4_years);
	y, n = divmod(n, days_in_year);
	local t = (y400 * 400) + (y100 * 100) + (y4 * 4) + y;
	if (y == 4) or (y100 == 4) then
		-- The extra day
		return t, 12, 31, 366;
	end;
	
	-- No months have 32 days
	local m = bit32.rshift(n, 5) + 1;
	if n >= months_to_days[m + 1] + (m > 1 and is_leap(t + 1) and 1 or 0) then
		m = m + 1;
	end;
	
	return t + 1, m, (n - (months_to_days[m] + (m > 1 and is_leap(t + 1) and 1 or 0))) + 1, n + 1;
end;

--[=[ Methods ]=]--
local d_methods = setmetatable(
	{ },
	{
		__newindex = function(self, index, func)
			rawset(self, index, function(self, ...)
				if not (dt_proxy[self] and dt_proxy[self].type ~= 0) then
					error("Expected ':' not '.' calling member function " .. index, 2);
				end;
				return func(self, ...);
			end);
		end;
	}
);

local check;
if typeof(intl) == "Instance" then
	check = require(intl:WaitForChild("argChecker"));
	intl = require(intl);
	function d_methods:ToLocaleDateString(locale, options)
		return intl.ToLocaleString(self, locale, check.check_options(options, 'dt/date'));
	end;
	d_methods.ToLocaleString = d_methods.ToLocaleDateString;
else
	intl = nil;
end;

local dt_methods = setmetatable(
	{ },
	{
		__index = d_methods;
		
		__newindex = function(self, index, func)
			rawset(self, index, function(self, ...)
				if not (dt_proxy[self] and dt_proxy[self].type == 1) then
					error("Expected ':' not '.' calling member function " .. index, 2);
				end;
				return func(self, ...);
			end);
		end;
	}
);
function dt_methods:ToISOString(...)
	if select('#', ...) > 0 then
		if (...) ~= 'T' and (...) ~= ' ' then
			if type((...)) ~= "string" and type((...)) ~= "number" then
				error("The separator must be 'T' or ' ', not " .. typeof((...)), 2);
			else
				error("The separator must be 'T' or ' ', not " .. (...), 2);
			end;
		end
	end;
	return ("%04d-%02d-%02d%s%02d:%02d:%02d.%03d%s"):format(self.Year, self.Month, self.Day, (...) or ' ',
		self.Hour, self.Minute, self.Second, self.Millisecond, offset_to_string(self.TimezoneOffset, ':', true));
end;

function dt_methods:ToISOUTCString(...)
	if select('#', ...) > 0 then
		if (...) ~= 'T' and (...) ~= ' ' then
			if type((...)) ~= "string" and type((...)) ~= "number" then
				error("The separator must be 'T' or ' ', not " .. typeof((...)), 2);
			else
				error("The separator must be 'T' or ' ', not " .. (...), 2);
			end;
		end
	end;
	return ("%04d-%02d-%02d%s%02d:%02d:%02d.%03dZ"):format(self.UtcYear, self.UtcMonth, self.UtcDay, (...) or ' ',
		self.UtcHour, self.UtcMinute, self.UtcSecond, self.UtcMillisecond);
end;

function dt_methods:ToRFCString()
	return ("%s, %02d %s %04d, %02d:%02d:%02d.%03d %s"):format(day_names[self.DayOfWeek], self.Day,
		month_names[self.Month], self.Year, self.Hour, self.Minute, self.Second, self.Millisecond, offset_to_string(self.TimezoneOffset, '', true));
end;

function dt_methods:ToRFCUTCString()
	return ("%s, %02d %s %04d, %02d:%02d:%02d.%03d +0000"):format(day_names[self.UtcDayOfWeek], self.UtcDay,
		month_names[self.UtcMonth], self.UtcYear, self.UtcHour, self.UtcMinute, self.UtcSecond, self.UtcMillisecond);
end;

function dt_methods:ToCString()
	return ("%s %s %2d %02d:%02d:%02d %04d"):format(day_names[self.DayOfWeek], month_names[self.Month], 
		self.Day, self.Hour, self.Minute, self.Second, self.Year);
end;

function dt_methods:Format(ptn, data)
	return format(formats.dt, data, false, self, ptn);
end;

function dt_methods:UtcFormat(ptn, data)
	return format(formats.dt, data, true, self, ptn);
end;

function dt_methods:ToOsDate()
	return os.date('!*t', self.Epoch);
end;

local function comp_d(safe, left, right)
	if (not dt_proxy[left]) or (not dt_proxy[right]) then
		if safe then
			return nil;
		end;
		error("attempt to compare " .. (typeof(left) .. ' and ' .. typeof(right)):gsub(' and ' .. typeof(left) .. '$', ''), 2);
	end;
	local left_tick = dt_proxy[left].ticks;
	local right_tick = dt_proxy[right].ticks;
	if dt_proxy[left].type ~= dt_proxy[right].type then
		if dt_proxy[left].type == 2 then
			left_tick = left_tick * 86400;
		else
			right_tick = right_tick * 86400;
		end;
		if dt_proxy[left].type == 0 or dt_proxy[right].type == 0 then
			if safe then
				return nil;
			end;
			error("attempt to compare userdata", 2);
		end;
	end;
	return ((left_tick == right_tick) and 0) or ((left_tick < right_tick) and -1 or 1);
end;
local function comp_ts(safe, left, right)
	if (not dt_proxy[left]) or (not dt_proxy[right]) then
		if safe then
			return nil;
		end;
		error("attempt to compare " .. (typeof(left) .. ' and ' .. typeof(right)):gsub(' and ' .. typeof(left) .. '$', ''), 2);
	end;
	if dt_proxy[left].type ~= 0 or dt_proxy[right].type ~= 0 then
		if safe then
			return nil;
		end;
		error("attempt to compare userdata", 2);
	end;
	local left_tick = dt_proxy[left].ticks;
	local right_tick = dt_proxy[right].ticks;
	return ((left_tick == right_tick) and 0) or ((left_tick < right_tick) and -1 or 1);
end;
function dt_methods:Compare(...)
	if select('#', ...) == 0 then
		error("missing argument #2", 2);
	end;
	return comp_d(false, self, (...));
end;
function dt_methods:AddDays(...)
	if select('#', ...) == 0 then
		error("bad argument #2 (number expected, got no value)");
	elseif type((...)) ~= "number" then
		error("bad arugment #2 (number expected, got " .. typeof((...)) .. ')');
	end;
	return self + ((...) * 86400);
end;
function dt_methods:AddMonths(...)
	if select('#', ...) == 0 then
		error("bad argument #2 (number expected, got no value)");
	elseif type((...)) ~= "number" then
		error("bad arugment #2 (number expected, got " .. typeof((...)) .. ')');
	end;
	return dt.new(self.Year, self.Month + (...), self.Day, self.Hour, self.Minute, self.Second, self.Millisecond);
end;
function dt_methods:AddYears(...)
	if select('#', ...) == 0 then
		error("bad argument #2 (number expected, got no value)");
	elseif type((...)) ~= "number" then
		error("bad arugment #2 (number expected, got " .. typeof((...)) .. ')');
	end;
	return self:AddMonths((...) * 12);
end;

if intl then
	d_methods.ToLocaleString = intl.ToLocaleString;
	function d_methods:ToLocaleTimeString(locale, options)
		return intl.ToLocaleString(self, locale, check.check_options(options, 'dt/time'));
	end;
end;

local ts_methods = setmetatable(
	{ },
	{
		__newindex = function(self, index, func)
			rawset(self, index, function(self, ...)
				if not (dt_proxy[self] and dt_proxy[self].type == 0) then
					error("Expected ':' not '.' calling member function " .. index, 2);
				end;
				return func(self, ...);
			end);
		end;
	}
);

function ts_methods:Abs()
	return rawnew(math.abs(self.TotalMilliseconds), 0);
end;
function ts_methods:Compare(...)
	if select('#', ...) == 0 then
		error("missing argument #2", 2);
	end;
	return comp_ts(false, self, (...));
end;

--[=[ Metamethods ]=]--
local dt_access_value =
{
	Year = 1, DayOfYear = 2, Month = 3, DayOfWeek = 4, Day = 5, TimeOfDay = 6,
	Hour = 7, Minute = 8, Second = 9, Millisecond = 10,
};
local ts_access_value =
{
	Days = 1, Hours = 2, Minutes = 3, Seconds = 4, Milliseconds = 5,
	TotalDays = 6, TotalHours = 7, TotalMinutes = 8, TotalSeconds = 9, TotalMilliseconds = 10,
	TotalIntHours = 11, TotalIntMinutes = 12, TotalIntSeconds = 13,
};
local function index_dt(self, index)
	if dt_methods[index] then
		return dt_methods[index];
	elseif index == "Epoch" then
		return dt_proxy[self].ticks - days_in_1970_years;
	elseif index == "Ticks" then
		return dt_proxy[self].ticks;
	elseif index == "TimezoneOffset" then
		return dt_proxy[self].access.offset;
	elseif dt_access_value[index] then
		return dt_proxy[self].access.lo[dt_access_value[index]];
	elseif type(index) == "string" and index:sub(1, 3) == "Utc" then
		local prop = index:sub(4);
		if dt_access_value[prop] then
			return dt_proxy[self].access.utc[dt_access_value[prop]];
		end;
	end;
	if type(index) ~= "string" and type(index) ~= "number" then
		error("invalid argument #2 (string expected, got " .. typeof(index) .. ')', 2);
	else
		error(tostring(index) .. " is not a valid member of DateTime", 2);
	end;
end;
local function index_ts(self, index)
	if ts_methods[index] then
		return ts_methods[index];
	elseif ts_access_value[index] then
		return dt_proxy[self].access[ts_access_value[index]];
	end;
	if type(index) ~= "string" and type(index) ~= "number" then
		error("invalid argument #2 (string expected, got " .. typeof(index) .. ')', 2);
	else
		error(tostring(index) .. " is not a valid member of TimeSpan", 2);
	end;
end;
local function newindex(self, index, value)
	error(index .. " cannot be assigned to", 2);
end;
local function add(left, right)
	local date, span;
	if type(left) == "number" or (dt_proxy[left] and dt_proxy[left].type == 0) then
		span = left;
		date = right;
	elseif type(right) == "number" or (dt_proxy[right] and dt_proxy[right].type == 0) then
		span = right;
		date = left;
	end;
	
	if (not span) or (not dt_proxy[date]) then
		error("attempt to perform arithmetic (add) on " .. (typeof(left) .. ' and ' .. typeof(right)):gsub(' and ' .. typeof(left) .. '$', ''), 2);
	end;
	return rawnew(dt_proxy[date].ticks + (dt_proxy[span] and math.floor(dt_proxy[span].ticks / (dt_proxy[date].type == 2 and 86400 or 1)) or span), dt_proxy[date].type);
end;
local function sub(left, right)
	local comb = (type(left) == "number" and 'ts' or (dt_proxy[left] and (dt_proxy[left].type == 0 and 'ts' or (dt_proxy[left].type == 1 and 'dt' or 'd')) or 'zz')) 
		.. '-' ..
		(type(right) == "number" and 'ts' or (dt_proxy[right] and (dt_proxy[right].type == 0 and 'ts' or (dt_proxy[right].type == 1 and 'dt' or 'd')) or 'zz'));
	if comb ~= 'dt-dt' and comb ~= 'dt-ts' and comb ~= 'ts-ts' and comb ~= 'd-d' and comb ~= 'd-ts' then
		error("attempt to perform arithmetic (sub) on " .. (typeof(left) .. ' and ' .. typeof(right)):gsub(' and ' .. typeof(left) .. '$', ''), 2);
	end;
	return rawnew(dt_proxy[left].ticks - (dt_proxy[right] and math.floor(dt_proxy[right].ticks / (comb == 'd-ts' and 86400 or 1)) or right), (comb == 'dt-ts' or comb == 'd-ts') and 1 or 0);
end;
local function mul_ts(left, right)
	if (not ((dt_proxy[left] and dt_proxy[left].type == 0) or type(left) == "number")) or (not ((dt_proxy[right] and dt_proxy[right].type == 0) or type(right) == "number")) then
		error("attempt to perform arithmetic (mul) on " .. (typeof(left) .. ' and ' .. typeof(right)):gsub(' and ' .. typeof(left) .. '$', ''), 2);
	end;
	return rawnew((dt_proxy[left] and dt_proxy[left].ticks or left) * (dt_proxy[right] and dt_proxy[right].ticks or right), 0);
end;
local function div_ts(left, right)
	if (not ((dt_proxy[left] and dt_proxy[left].type == 0) or type(left) == "number")) or (not ((dt_proxy[right] and dt_proxy[right].type == 0) or type(right) == "number")) then
		error("attempt to perform arithmetic (div) on " .. (typeof(left) .. ' and ' .. typeof(right)):gsub(' and ' .. typeof(left) .. '$', ''), 2);
	end;
	return rawnew(math.floor((dt_proxy[left] and dt_proxy[left].ticks or left) / (dt_proxy[right] and dt_proxy[right].ticks or right)), 0);
end;
local function mod_ts(left, right)
	if (not ((dt_proxy[left] and dt_proxy[left].type == 0) or type(left) == "number")) or (not ((dt_proxy[right] and dt_proxy[right].type == 0) or type(right) == "number")) then
		error("attempt to perform arithmetic (mod) on " .. (typeof(left) .. ' and ' .. typeof(right)):gsub(' and ' .. typeof(left) .. '$', ''), 2);
	end;
	return rawnew((dt_proxy[left] and dt_proxy[left].ticks or left) % (dt_proxy[right] and dt_proxy[right].ticks or right), 0);
end;
local function unm_ts(self)
	return rawnew(self.TotalMilliseconds * -1, 0);
end;
local function eq_d(left, right)
	return comp_d(true, left, right) == 0;
end;
local function lt_d(left, right)
	return comp_d(false, left, right) < 0;
end;
local function le_d(left, right)
	return comp_d(false, left, right) <= 0;
end;
local function eq_ts(left, right)
	return comp_ts(true, left, right) == 0;
end;
local function lt_ts(left, right)
	return comp_ts(false, left, right) < 0;
end;
local function le_ts(left, right)
	return comp_ts(false, left, right) <= 0;
end;

--[=[ Private function continuted ]=]--
local function parse_date(self)
	-- Check ISO 8601
	local y, M, d, h, m, s, f, o = self:match("(%d%d%d%d%d*)%-(%d%d)%-(%d%d)[T%s](%d%d):(%d%d):(%d%d).?(%d?%d?%d?)([%+%-Z]%d?%d?:?%d?%d?)");
	local match = y and (#f == 3 or #f == 0) and (o:match("[%+%-]%d%d:?%d%d") or o == 'Z');
	local o_h, o_m;
	if match and o ~= '' then
		o_h, o_m = o:match("([%+%-]%d%d):?(%d%d)");
		if o_h:sub(1, 1) == '-' then
			o_m = '-' .. o_m;
		end;
	end;
	if not match then
		-- Check RFC 2822
		local E;
		E, d, M, y, h, m, s, o = self:match("([A-Z][a-z][a-z]),?%s(%d%d)%s([A-Z][a-z][a-z]),?%s(%d%d):(%d%d):(%d%d)%s([%+%-]%d%d%d%d)");
		M = table.find(month_names, M);
		match = table.find(day_names, E) and M;
		if match then
			o_h, o_m = o:match("([%+%-]%d%d)(%d%d)");
			if o_h:sub(1, 1) == '-' then
				o_m = '-' .. o_m;
			end;
		end;
	end;
	
	if match then
		return tonumber(y), tonumber(M) or 1, tonumber(d) or 1, 
			(tonumber(h) or 0) - (o_h or 0), (tonumber(m) or 0) - (o_m or 0),
			tonumber(s) or 0, tonumber(f) or 0;
	end;
	
	local token = { };
	for t in self:gsub("(%D+)", " %1 "):gmatch("[^%p%s]+") do
		table.insert(token, (t:sub(0, 2) ~= '00' and tonumber(t)) or t);
	end;
	if #token < 2 or #token > 7 or #token == 4 then
		return nil;
	end;
	local pm;
	local yp, mp, dp, hp, np, sp, fp;
	for i, v in ipairs(token) do
		if type(v) == "string" then
			v = v:sub(1, 1):upper() .. v:sub(2):lower();
			if table.find(day_names, v) or table.find(full_day_names, v) then
				continue;
			elseif (table.find(month_names, v) or table.find(full_month_names, v)) and not mp then
				mp = i;
			elseif v == "Am" or v == "Pm" then
				pm = v == "Pm";
			elseif tonumber(v) then
				if yp then
					return nil;
				end;
				-- 4 digit year
				yp = i;
			else
				return nil;
			end;
		else
			if v % 1 ~= 0 or v < 0 then
				return nil;
			elseif v >= 1000 and v < 100000 then
				if yp then
					return nil;
				end;
				yp = i;
			end;
		end;
	end;
	local precded_by_time = (yp == #token or yp == #token - 2);
	if #token > 3 and precded_by_time then
		if mp then
			if mp < 3 then
				dp = (mp == 1) and 2 or ((yp == 1) and 3 or 1);
			else
				dp = (mp == #token - 2) and #token - 1 or ((yp == #token) and #token - 2 or #token);
			end;
		else
			mp, dp = #token - 1, (yp == #token) and #token - 2 or #token;
		end;
		local tp = (mp < 3) and #token or #token - 2;
		if #token == 5 then
			np, hp = tp - 1, tp - 2;
		elseif #token == 6 then
			sp, np, hp = tp - 1, tp - 2, tp - 3;
		else
			fp, sp, np, hp = tp - 1, tp - 2, tp - 3, tp - 4;
		end;
	elseif mp then
		dp = (mp == 1) and 2 or ((yp == 1) and 3 or 1);
	else
		mp, dp = 2, (yp == 1) and 3 or 1;
	end;
	local month = token[mp or 2];
	local hour = token[hp or 4];
	if pm ~= nil then
		if hour > 12 or hour < 0 then
			return nil;
		end;
		hour = (hour % 12) + (pm and 12 or 0);
	end;
	return tonumber(token[yp or 1]), table.find(month_names, month) or table.find(full_month_names, month) or month or 1, token[dp or 3], hour, token[np or 5], token[sp or 6], token[fp or 7];
end;

local function getoffset()
	-- I'm sure by 10'000 AD they'll figure it out
	return math.floor((os.time(os.date('*t')) - os.time(os.date('!*t'))) / 60);
end;

function rawnew(ticks, ttype, access)
	if not access then
		if ttype == 0 then
			local f, s, m, h, d;
			local d = math.floor(ticks);
			d, f = divmod(d, 1000);
			d, s = divmod(d, 60);
			d, m = divmod(d, 60);
			d, h = divmod(d, 24);
			access =
			{
				d, h, m, s, f,
				ticks / 86400000, ticks / 3600000, ticks / 60000, ticks / 1000, ticks,
				math.floor(ticks / 3600000), math.floor(ticks / 60000), math.floor(ticks / 1000)
			};
		elseif ttype == 1 then
			local f, s, m, h, M, D, y;
			local d = math.floor(ticks);
			local time_of_day = d % 86400000;
			d, f = divmod(d, 1000);
			d, s = divmod(d, 60);
			d, m = divmod(d, 60);
			d, h = divmod(d, 24);
			local E = (d % 7) + 1;
			y, M, d, D = to_date(d, 2);
			local utc_access = { y, D, M, E, d, time_of_day, h, m, s, f };
			
			local offset = getoffset();
			d = math.floor(ticks) + (offset * 60000);
			time_of_day = d % 86400000;
			d, f = divmod(d, 1000);
			d, s = divmod(d, 60);
			d, m = divmod(d, 60);
			d, h = divmod(d, 24);
			E = (d % 7) + 1;
			y, M, d, D = to_date(d, 2);
			access = { lo = { y, D, M, E, d, time_of_day, h, m, s, f }, utf = utc_access, offset = offset };
		end;
	end;
	-- Return nil if it's out of the range
	if ticks < (min_val - days_in_1970_years) or ticks > (max_val - days_in_1970_years) then
		return nil;
	end;
	local proxy = newproxy(true);
	dt_proxy[proxy] = { type = ttype, ticks = ticks, access = access };
	
	local proxy_mt = getmetatable(proxy);
	proxy_mt.__index = index_dt;
	proxy_mt.__newindex = newindex;
	proxy_mt.__add = add;
	proxy_mt.__sub = sub;
	if ttype == 0 then
		proxy_mt.__mul = mul_ts;
		proxy_mt.__div = div_ts;
		proxy_mt.__mod = mod_ts;
		proxy_mt.__unm = unm_ts;
	end;
	proxy_mt.__le = ttype == 0 and le_ts or le_d;
	proxy_mt.__lt = ttype == 0 and lt_ts or lt_d;
	proxy_mt.__eq = ttype == 0 and eq_ts or eq_d;
	proxy_mt.__tostring = dt_methods.ToRFCString;
	proxy_mt.__metatable = "The metatable is locked";
	return proxy;
end;

--[=[ DateTime ]=]--
function dt.new(...)
	local arglen = select('#', ...);
	local args, ret = { ... }, { };
	if arglen == 0 then
		-- Void not nil
		return dt.Now();
	elseif arglen == 1 then
		-- Void not nil
		if type(args[1]) == 'string' then
			return dt.new(parse_date(args[1]));
		elseif type(args[1]) == 'number' then
			if args[1] < min_val or args[1] > max_val then
				return nil;
			end;
			return rawnew(args[1] + (days_in_1970_years * 86400000), 1);
		elseif type(args[1]) == 'table' then
			if type(args[1].Year or args[1].year) == 'table' then
				return nil;
			end;
			return dt.new(
				args[1].Year or args[1].year,
				args[1].Month or args[1].month,
				args[1].Day or args[1].day,
				args[1].Hour or args[1].hour,
				args[1].Minute or args[1].min,
				args[1].Second or args[1].sec,
				args[1].Millisecond
			);
		elseif dt_proxy[args[1]] then
			if dt_proxy[args[1]].type == 1 then
				-- re-creating the object
				return rawnew(dt_proxy[args[1]].ticks, 1, dt_proxy[args[1]].access);
			elseif dt_proxy[args[1]].type == 2 then
				return dt.new(args[1].Year, args[1].Month, args[1].Day);
			else
				return dt.new(args[1].TotalMilliseconds);
			end;
		end;
	end;
	for index = 1, 7 do
		local value = args[index];
		if index <= arglen then
			value = tonumber(value);
			if not value then
				return nil;
			end;
			ret[index] = math.floor(value);
		else
			if index == 1 then
				return nil;
			end;
			ret[index] = (index < 4) and 1 or 0;
		end;
	end;
	local y, M, d, h, m, s, f = unpack(ret);
	local y_offset, M_offset = divmod(M - 1, 12);
	-- Fix the date if any values go over or below the range.
	y, M = y + y_offset, M_offset + 1;
	local days = from_date(y, M, 1, false) + (d - 1);
	local seconds = (h * 3600) + (m * 60) + s;
	local ticks = (days * 86400000) + (seconds * 1000) + f;
	local offset = getoffset();
	local utc_access = { y, from_date(y, M, d, true), M, days % 7, d, seconds, h, m, s, f };
	
	-- Convert to local time
	local d = ticks + (offset * 60000);
	local time_of_day = d % 86400;
	local D;
	d, f = divmod(d, 1000);
	d, s = divmod(d, 60);
	d, m = divmod(d, 60);
	d, h = divmod(d, 24);
	local E = (d % 7) + 1;
	y, M, d, D = to_date(d, 2);
	
	return rawnew(ticks, 1, { lo = { y, D, M, E, d, time_of_day, h, m, s, f }, utc = utc_access, offset = offset });
end;

-- https://devforum.roblox.com/t/is-there-any-way-to-get-the-exact-millisecond-of-now-in-utc-from-a-server/526492
-- Mix both os.time and tick for milliseconds, may not be consistent but it's better than nothing
-- It'll get the timezone offset automatically
function dt.Now()
	return dt.new((os.time() * 1000) + math.floor((tick() * 1000) % 1000));
end;

--[=[ TimeSpan ]=]--
local ts = { };
function ts.new(...)
	local d, h, m, s, f = 0, 0, 0, 0, 0;
	local arglen = select('#', ...);
	if arglen == 1 then
		f = ...;
	elseif arglen == 2 then
		s, f = ...;
	elseif arglen == 3 then
		h, m, s = ...;
	elseif arglen == 4 then
		d, h, m, s = ...;
	else
		d, h, m, s, f = ...;
	end;
	d, h, m, s, f = tonumber(d), tonumber(h), tonumber(m), tonumber(s), tonumber(f);
	if not (d and h and m and s and f) then
		return nil;
	end;
	return rawnew((d * 86400000) + (h * 3600000) + (m * 60000) + (s * 1000) + f, 0);
end;

function ts.FromSeconds(v)
	return ts.new(0, 0, v);
end;

function ts.FromMinutes(v)
	return ts.new(0, v, 0);
end;

function ts.FromHours(v)
	return ts.new(v, 0, 0);
end;

function ts.FromDays(v)
	return ts.new(v, 0, 0, 0);
end;

return { DateTime = dt, TimeSpan = ts };
