// Hello, Please Excuse my terrible Coding
#ifndef _MSC_VER
#define _ISOC11_SOURCE /* for aligned_alloc() */
#endif

#define VKCPP_DISABLE_ENHANCED_MODE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <iostream>
#include "vk_cpp.h"
#include <vulkan/vulkan.h>

#ifdef _WIN32
#pragma comment(linker, "/subsystem:windows")
#define APP_NAME_STR_LEN 80
#endif // _WIN32

#define DEMO_TEXTURE_COUNT 1
#define VERTEX_BUFFER_BIND_ID 0
#define APP_SHORT_NAME "Vulkan_Remake"
#define APP_LONG_NAME "The Vulkan Triangle Demo Program"

#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))

#if defined(NDEBUG) && defined(__GNUC__)
#define U_ASSERT_ONLY __attribute__((unused))
#else
#define U_ASSERT_ONLY
#endif

#ifdef _WIN32
#define ERR_EXIT(err_msg, err_class)                                           \
    do {                                                                       \
        MessageBox(NULL, err_msg, err_class, MB_OK);                           \
        exit(1);                                                               \
    } while (0)
#else // _WIN32

#define ERR_EXIT(err_msg, err_class)                                           \
    do {                                                                       \
        printf(err_msg);                                                       \
        fflush(stdout);                                                        \
        exit(1);                                                               \
    } while (0)
#endif // _WIN32

struct texture_object {
	vk::Sampler sampler;

	vk::Image image;
	vk::ImageLayout imageLayout;

	vk::DeviceMemory mem;
	vk::ImageView view;

	int32_t tex_width, tex_height;
};

VKAPI_ATTR VkBool32 VKAPI_CALL
dbgFunc(VkFlags msgFlags, VkDebugReportObjectTypeEXT objType,
	uint64_t srcObject, size_t location, int32_t msgCode,
	const char *pLayerPrefix, const char *pMsg, void *pUserData) {
	char *message = (char *)malloc(strlen(pMsg) + 100);

	assert(message);

	if (msgFlags & VK_DEBUG_REPORT_ERROR_BIT_EXT) {
		sprintf(message, "ERROR: [%s] Code %d : %s", pLayerPrefix, msgCode,
			pMsg);
	}
	else if (msgFlags & VK_DEBUG_REPORT_WARNING_BIT_EXT) {
		sprintf(message, "WARNING: [%s] Code %d : %s", pLayerPrefix, msgCode,
			pMsg);
	}
	else {
		return false;
	}

#ifdef _WIN32
	MessageBox(NULL, message, "Alert", MB_OK);
#else
	printf("%s\n", message);
	fflush(stdout);
#endif
	free(message);

	/*
	* false indicates that layer should not bail-out of an
	* API call that had validation failures. This may mean that the
	* app dies inside the driver due to invalid parameter(s).
	* That's what would happen without validation layers, so we'll
	* keep that behavior here.
	*/
	return false;
}

typedef struct _SwapchainBuffers {
	vk::Image image;
	vk::CommandBuffer cmd;
	vk::ImageView view;
} SwapchainBuffers;

struct demoStruct {
#ifdef _WIN32
#define APP_NAME_STR_LEN 80
	HINSTANCE connection;        // hInstance - Windows Instance
	char name[APP_NAME_STR_LEN]; // Name to put on the window/icon
	HWND window;                 // hWnd - window handle
#else                            // _WIN32
	xcb_connection_t *connection;
	xcb_screen_t *screen;
	xcb_window_t window;
	xcb_intern_atom_reply_t *atom_wm_delete_window;
#endif                           // _WIN32
	vk::SurfaceKHR surface;
	bool prepared;
	bool use_staging_buffer;

	vk::Instance inst;
	vk::PhysicalDevice gpu;
	vk::Device device;
	vk::Queue queue;
	vk::PhysicalDeviceProperties gpu_props;
	vk::QueueFamilyProperties *queue_props;
	uint32_t graphics_queue_node_index;

	uint32_t enabled_extension_count;
	uint32_t enabled_layer_count;
	char *extension_names[64];
	char *device_validation_layers[64];

	int width, height;
	vk::Format format;
	vk::ColorSpaceKHR color_space;

	PFN_vkGetPhysicalDeviceSurfaceSupportKHR fpGetPhysicalDeviceSurfaceSupportKHR;
	PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR fpGetPhysicalDeviceSurfaceCapabilitiesKHR;
	PFN_vkGetPhysicalDeviceSurfaceFormatsKHR fpGetPhysicalDeviceSurfaceFormatsKHR;
	PFN_vkGetPhysicalDeviceSurfacePresentModesKHR fpGetPhysicalDeviceSurfacePresentModesKHR;

	PFN_vkCreateSwapchainKHR fpCreateSwapchainKHR;
	PFN_vkDestroySwapchainKHR fpDestroySwapchainKHR;
	PFN_vkGetSwapchainImagesKHR fpGetSwapchainImagesKHR;
	PFN_vkAcquireNextImageKHR fpAcquireNextImageKHR;
	PFN_vkQueuePresentKHR fpQueuePresentKHR;

	uint32_t swapchainImageCount;
	vk::SwapchainKHR swapchain = VK_NULL_HANDLE;
	SwapchainBuffers *buffers;

	vk::CommandPool cmd_pool;

	struct {
		vk::Format format;

		vk::Image image;
		vk::DeviceMemory mem;
		vk::ImageView view;
	} depth;

	struct texture_object textures[DEMO_TEXTURE_COUNT];

	struct {
		vk::Buffer buf;
		vk::DeviceMemory mem;

		vk::PipelineVertexInputStateCreateInfo vi;
		vk::VertexInputBindingDescription vi_bindings[1];
		vk::VertexInputAttributeDescription vi_attrs[2];
	} vertices;

	vk::CommandBuffer setup_cmd; // Command Buffer for initialization commands
	vk::CommandBuffer draw_cmd;  // Command Buffer for drawing commands
	vk::PipelineLayout pipeline_layout;
	vk::DescriptorSetLayout desc_layout;
	vk::PipelineCache pipelineCache;
	vk::RenderPass render_pass;
	vk::Pipeline pipeline;

	vk::ShaderModule vert_shader_module;
	vk::ShaderModule frag_shader_module;

	vk::DescriptorPool desc_pool;
	vk::DescriptorSet desc_set;

	vk::Framebuffer *framebuffers;

	vk::PhysicalDeviceMemoryProperties memory_properties;

	bool validate = true;
	PFN_vkCreateDebugReportCallbackEXT CreateDebugReportCallback;
	PFN_vkDestroyDebugReportCallbackEXT DestroyDebugReportCallback;
	VkDebugReportCallbackEXT msg_callback;
	PFN_vkDebugReportMessageEXT DebugReportMessage;

	float depthStencil;
	float depthIncrement;

	bool quit;
	uint32_t current_buffer;
	uint32_t queue_count;

	/*demoStruct::demoStruct()
	:
	surface(VkSurfaceKHR()),
	prepared(false),
	use_staging_buffer(true),
	inst(VkInstance()),
	gpu(VkPhysicalDevice()),
	device(VkDevice()),
	queue(VkQueue()),
	graphics_queue_node_index(0),

	gpu_props(vk::PhysicalDeviceProperties()),
	queue_props(&vk::QueueFamilyProperties()),
	memory_properties(vk::PhysicalDeviceMemoryProperties()),

	enabled_extension_count(0),
	enabled_layer_count(0),

	extension_names{},
	device_validation_layers{},

	width(500),
	height(500),

	format(),
	color_space(),

	fpGetPhysicalDeviceSurfaceSupportKHR(PFN_vkGetPhysicalDeviceSurfaceSupportKHR()),
	fpGetPhysicalDeviceSurfaceCapabilitiesKHR(PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR()),
	fpGetPhysicalDeviceSurfaceFormatsKHR(PFN_vkGetPhysicalDeviceSurfaceFormatsKHR()),
	fpGetPhysicalDeviceSurfacePresentModesKHR(PFN_vkGetPhysicalDeviceSurfacePresentModesKHR()),

	fpCreateSwapchainKHR(PFN_vkCreateSwapchainKHR()),
	fpDestroySwapchainKHR(PFN_vkDestroySwapchainKHR()),
	fpGetSwapchainImagesKHR(PFN_vkGetSwapchainImagesKHR()),
	fpAcquireNextImageKHR(PFN_vkAcquireNextImageKHR()),
	fpQueuePresentKHR(PFN_vkQueuePresentKHR()),
	swapchainImageCount(1),
	swapchain(VK_NULL_HANDLE),
	buffers(&SwapchainBuffers()),

	cmd_pool(VK_NULL_HANDLE),

	depth{
	vk::Format(),
	vk::Image(),
	vk::DeviceMemory(),
	vk::ImageView()
	},

	textures{},

	pipeline_layout(VkPipelineLayout()),
	desc_layout(VkDescriptorSetLayout()),
	pipelineCache(VkPipelineCache()),
	render_pass(vk::RenderPass()),
	pipeline(VkPipeline()),

	vert_shader_module(VkShaderModule()),
	frag_shader_module(VkShaderModule()),

	desc_pool(VkDescriptorPool()),
	desc_set(VkDescriptorSet()),

	framebuffers(),

	quit(),
	validate(true),
	CreateDebugReportCallback(),
	DestroyDebugReportCallback(),
	DebugReportMessage(),
	current_buffer(),
	queue_count(),
	msg_callback(VkDebugReportCallbackEXT())
	{

	}*/
};


// Forward declaration:
static void demo_resize(demoStruct* demo);

static bool memory_type_from_properties(
	demoStruct *demo,
	uint32_t typeBits,
	vk::MemoryPropertyFlags requirements_mask,
	uint32_t *typeIndex
	) {
	// Search memtypes to find first index with those properties
	for (uint32_t i = 0; i < 32; i++) {
		if ((typeBits & 1) == 1) {
			// Type is available, does it match user properties?
			if ((demo->memory_properties.memoryTypes[i].propertyFlags &
				requirements_mask) == requirements_mask) {
				*typeIndex = i;
				return true;
			}
		}
		typeBits >>= 1;
	}
	// No memory types matched, return failure
	return false;
}

// Checked 6/8/2016
static void demo_flush_init_cmd(demoStruct* demo) {
	vk::Result U_ASSERT_ONLY err;

	if (!reinterpret_cast<bool &>(demo->setup_cmd))
		return;

	err = demo->setup_cmd.end();
	assert(err == vk::Result::eSuccess);

	const vk::CommandBuffer cmd_bufs[] = { demo->setup_cmd };
	vk::Fence nullFence = { VK_NULL_HANDLE };
	vk::SubmitInfo submit_info(0, NULL, NULL, 1, cmd_bufs, 0, NULL);

	err = demo->queue.submit(1, &submit_info, nullFence);
	assert(err == vk::Result::eSuccess);

	err = demo->queue.waitIdle();
	assert(err == vk::Result::eSuccess);

	demo->device.freeCommandBuffers(demo->cmd_pool, 1, cmd_bufs);
	demo->setup_cmd = VK_NULL_HANDLE;
}


// Checked 6/5/2016
static void demo_set_image_layout(demoStruct* demo,
	vk::Image image,
	vk::ImageAspectFlags aspectMask,
	vk::ImageLayout old_image_layout,
	vk::ImageLayout new_image_layout,
	vk::AccessFlagBits srcAccessMask) {

	vk::Result U_ASSERT_ONLY err;

	if (!reinterpret_cast<bool &>(demo->setup_cmd)) {
		const vk::CommandBufferAllocateInfo cmd(
			demo->cmd_pool, vk::CommandBufferLevel::ePrimary, 1);

		err = demo->device.allocateCommandBuffers(&cmd, &demo->setup_cmd);
		assert(err == vk::Result::eSuccess);

		vk::CommandBufferInheritanceInfo cmd_buf_hinfo
			(vk::RenderPass(), 0, vk::Framebuffer(), VK_FALSE, vk::QueryControlFlags(), vk::QueryPipelineStatisticFlags());

		vk::CommandBufferBeginInfo cmd_buf_info
			(vk::CommandBufferUsageFlags(), &cmd_buf_hinfo);

		err = demo->setup_cmd.begin(&cmd_buf_info);
		assert(err == vk::Result::eSuccess);
	}

	vk::ImageMemoryBarrier image_memory_barrier(
		srcAccessMask,
		vk::AccessFlags(),
		old_image_layout,
		new_image_layout,
		0,
		0,
		image,
		vk::ImageSubresourceRange(aspectMask, 0, 1, 0, 1)
		);

	if (new_image_layout == vk::ImageLayout::eTransferDstOptimal) {
		/* Make sure anything that was copying from this image has completed */
		image_memory_barrier.setDstAccessMask(vk::AccessFlagBits::eTransferRead);
	}

	if (new_image_layout == vk::ImageLayout::eColorAttachmentOptimal) {
		image_memory_barrier.setDstAccessMask
			(vk::AccessFlagBits::eColorAttachmentWrite);
	}

	if (new_image_layout == vk::ImageLayout::eDepthStencilAttachmentOptimal) {
		image_memory_barrier.setDstAccessMask
			(vk::AccessFlagBits::eDepthStencilAttachmentWrite);
	}

	if (new_image_layout == vk::ImageLayout::eShaderReadOnlyOptimal) {
		/* Make sure any Copy or CPU writes to image are flushed */
		image_memory_barrier.setDstAccessMask
			(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eInputAttachmentRead);
	}

	vk::ImageMemoryBarrier *pmemory_barrier = &image_memory_barrier;

	demo->setup_cmd.pipelineBarrier(
		vk::PipelineStageFlagBits::eTopOfPipe,
		vk::PipelineStageFlagBits::eTopOfPipe,
		vk::DependencyFlags(),
		0,
		NULL,
		0,
		NULL,
		1,
		pmemory_barrier
	);
}

//Checked 6/8/16
static void demo_get_entrypoints(demoStruct* demo) {
	demo->fpGetPhysicalDeviceSurfaceCapabilitiesKHR =
		reinterpret_cast<PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR>(demo->inst.getProcAddr("vkGetPhysicalDeviceSurfaceCapabilitiesKHR"));
	demo->fpGetPhysicalDeviceSurfaceFormatsKHR =
		reinterpret_cast<PFN_vkGetPhysicalDeviceSurfaceFormatsKHR>(demo->inst.getProcAddr("vkGetPhysicalDeviceSurfaceFormatsKHR"));
	demo->fpGetPhysicalDeviceSurfacePresentModesKHR =
		reinterpret_cast<PFN_vkGetPhysicalDeviceSurfacePresentModesKHR>(demo->inst.getProcAddr("vkGetPhysicalDeviceSurfacePresentModesKHR"));
	demo->fpGetPhysicalDeviceSurfaceSupportKHR =
		reinterpret_cast<PFN_vkGetPhysicalDeviceSurfaceSupportKHR>(demo->inst.getProcAddr("vkGetPhysicalDeviceSurfaceSupportKHR"));
	demo->fpCreateSwapchainKHR =
		reinterpret_cast<PFN_vkCreateSwapchainKHR>(demo->inst.getProcAddr("vkCreateSwapchainKHR"));
	demo->fpDestroySwapchainKHR =
		reinterpret_cast<PFN_vkDestroySwapchainKHR>(demo->inst.getProcAddr("vkDestroySwapchainKHR"));
	demo->fpGetSwapchainImagesKHR =
		reinterpret_cast<PFN_vkGetSwapchainImagesKHR>(demo->inst.getProcAddr("vkGetSwapchainImagesKHR"));
	demo->fpAcquireNextImageKHR =
		reinterpret_cast<PFN_vkAcquireNextImageKHR>(demo->inst.getProcAddr("vkAcquireNextImageKHR"));
	demo->fpQueuePresentKHR =
		reinterpret_cast<PFN_vkQueuePresentKHR>(demo->inst.getProcAddr("vkQueuePresentKHR"));

	if (demo->fpGetPhysicalDeviceSurfaceCapabilitiesKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpGetPhysicalDeviceSurfaceFormatsKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpGetPhysicalDeviceSurfacePresentModesKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpGetPhysicalDeviceSurfaceSupportKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpCreateSwapchainKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpDestroySwapchainKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpGetSwapchainImagesKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpAcquireNextImageKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
	if (demo->fpQueuePresentKHR == VK_NULL_HANDLE) { std::cout << "ERROR\n"; };
}

//###############################################################################################################################################################################################################################
// Checked 6/8/2016
static void demo_draw_build_cmd(demoStruct* demo) {
	const vk::CommandBufferInheritanceInfo cmd_buf_hinfo(
		//(RenderPass(), 0, Framebuffer(), 0, QueryControlFlags(), QueryPipelineStatisticFlags())

		VK_NULL_HANDLE,
		0,
		VK_NULL_HANDLE,
		VK_FALSE,
		vk::QueryControlFlags(),
		vk::QueryPipelineStatisticFlags()
		);
	const vk::CommandBufferBeginInfo cmd_buf_info
		(

			vk::CommandBufferUsageFlags(),
			&cmd_buf_hinfo
			);
	const vk::ClearValue clear_values[2] = {
		vk::ClearColorValue(std::array<float, 4>{ 0.2f, 0.2f, 0.2f, 0.2f }),
		vk::ClearDepthStencilValue(1.0f, 0)
	};
	const vk::RenderPassBeginInfo rp_begin(
		demo->render_pass,
		demo->framebuffers[demo->current_buffer],
		vk::Rect2D(
			vk::Offset2D(0, 0),
			vk::Extent2D(demo->width, demo->height)
			),
		2,
		clear_values
		);

	vk::Result U_ASSERT_ONLY err;

	err = demo->draw_cmd.begin(&cmd_buf_info);
	assert(err == vk::Result::eSuccess);

	demo->draw_cmd.beginRenderPass(&rp_begin, vk::SubpassContents::eInline);
	demo->draw_cmd.bindPipeline(vk::PipelineBindPoint::eGraphics,
		demo->pipeline);
	demo->draw_cmd.bindDescriptorSets(
		vk::PipelineBindPoint::eGraphics,
		demo->pipeline_layout, 0, 1, &demo->desc_set, 0,
		NULL);

	//vk::Viewport viewport;
	//memset(&viewport, 0, sizeof(viewport));
	vk::Viewport viewport = vk::Viewport(0, 0, (float)demo->width, (float)demo->height, (float)0.0f, (float)1.0f);








	demo->draw_cmd.setViewport(0, 1, &viewport);

	//vk::Rect2D scissor;
	//memset(&scissor, 0, sizeof(scissor));
	vk::Rect2D scissor = vk::Rect2D(vk::Offset2D(0, 0), vk::Extent2D(demo->width, demo->height));



	demo->draw_cmd.setScissor(0, 1, &scissor);

	vk::DeviceSize offsets[] = { 0 };
	demo->draw_cmd.bindVertexBuffers(VERTEX_BUFFER_BIND_ID, 1,
		&demo->vertices.buf, offsets);

	demo->draw_cmd.draw(3, 1, 0, 0);
	demo->draw_cmd.endRenderPass();

	vk::ImageMemoryBarrier prePresentBarrier(


		vk::AccessFlagBits::eColorAttachmentWrite,
		vk::AccessFlagBits::eMemoryRead,
		vk::ImageLayout::eColorAttachmentOptimal,
		vk::ImageLayout::ePresentSrcKHR,
		VK_QUEUE_FAMILY_IGNORED,
		VK_QUEUE_FAMILY_IGNORED,
		VK_NULL_HANDLE,
		vk::ImageSubresourceRange(vk::ImageAspectFlagBits::eColor, 0, 1, 0, 1));
	vk::ImageMemoryBarrier *pmemory_barrier = &prePresentBarrier;

	prePresentBarrier.setImage(demo->buffers[demo->current_buffer].image);
	demo->draw_cmd.pipelineBarrier(
		vk::PipelineStageFlagBits::eAllCommands,
		vk::PipelineStageFlagBits::eBottomOfPipe, vk::DependencyFlags(), 0, NULL, 0,
		NULL, 1, pmemory_barrier);

	err = demo->draw_cmd.end();
	assert(err == vk::Result::eSuccess);
}

// Checked 6/8/2016
static void demo_draw(demoStruct* demo) {
	vk::Result U_ASSERT_ONLY err;
	vk::Semaphore presentCompleteSemaphore;
	vk::SemaphoreCreateInfo presentCompleteSemaphoreCreateInfo = vk::SemaphoreCreateInfo(vk::SemaphoreCreateFlags());

	err = demo->device.createSemaphore(
		&presentCompleteSemaphoreCreateInfo,
		NULL, &presentCompleteSemaphore);
	assert(err == vk::Result::eSuccess);

	// Get the index of the next available swapchain image:
	VkResult err2 = demo->fpAcquireNextImageKHR(
		reinterpret_cast<VkDevice &>(demo->device),
		reinterpret_cast<VkSwapchainKHR &>(demo->swapchain), UINT64_MAX,
		reinterpret_cast<VkSemaphore &>(presentCompleteSemaphore),
		(VkFence)0, // TODO: Show use of fence
		&demo->current_buffer); assert(!err2);
	if (err == vk::Result::eErrorOutOfDateKHR) {
		// demo->swapchain is out of date (e.g. the window was resized) and
		// must be recreated:
		demo_resize(demo);
		demo_draw(demo);
		demo->device.destroySemaphore(presentCompleteSemaphore, NULL);
		return;
	}
	else if (err == vk::Result::eSuboptimalKHR) {
		// demo->swapchain is not as optimal as it could be, but the platform's
		// presentation engine will still present the image correctly.
	}
	else {
		assert(err == vk::Result::eSuccess);
	}

	// Assume the command buffer has been run on current_buffer before so
	// we need to set the image layout back to COLOR_ATTACHMENT_OPTIMAL
	demo_set_image_layout(demo, demo->buffers[demo->current_buffer].image,
		vk::ImageAspectFlagBits::eColor,
		vk::ImageLayout::ePresentSrcKHR,
		vk::ImageLayout::eColorAttachmentOptimal,
		vk::AccessFlagBits());
	demo_flush_init_cmd(demo);

	// Wait for the present complete semaphore to be signaled to ensure
	// that the image won't be rendered to until the presentation
	// engine has fully released ownership to the application, and it is
	// okay to render to the image.

	// FIXME/TODO: DEAL WITH VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
	demo_draw_build_cmd(demo);
	vk::Fence nullFence = VK_NULL_HANDLE;
	vk::PipelineStageFlags pipe_stage_flags =
		vk::PipelineStageFlagBits::eBottomOfPipe;
	vk::SubmitInfo submit_info(

		1,
		&presentCompleteSemaphore,
		&pipe_stage_flags,
		1,
		&demo->draw_cmd,
		0,
		NULL);

	err = demo->queue.submit(1, &submit_info, nullFence);
	assert(err == vk::Result::eSuccess);

	vk::PresentInfoKHR present(
		0,
		NULL,
		1,
		&demo->swapchain,
		&demo->current_buffer, NULL
		);

	// TBD/TODO: SHOULD THE "present" PARAMETER BE "const" IN THE HEADER?
	err2 = demo->fpQueuePresentKHR(
		reinterpret_cast<VkQueue&>(demo->queue),
		&reinterpret_cast<VkPresentInfoKHR &>(present)); err = vk::Result(err2);
	if (err == vk::Result::eErrorOutOfDateKHR) {
		// demo->swapchain is out of date (e.g. the window was resized) and
		// must be recreated:
		demo_resize(demo);
	}
	else if (err == vk::Result::eSuboptimalKHR) {
		// demo->swapchain is not as optimal as it could be, but the platform's
		// presentation engine will still present the image correctly.
	}
	else {
		assert(err == vk::Result::eSuccess);
	}

	err = demo->queue.waitIdle();
	assert(err == vk::Result::eSuccess);

	demo->device.destroySemaphore(presentCompleteSemaphore, NULL);
}

//Checked 6/8/16
static void demo_prepare_buffers(demoStruct* demo) {
	vk::Result U_ASSERT_ONLY err;
	VkResult U_ASSERT_ONLY err1;
	vk::SwapchainKHR oldSwapchain = demo->swapchain;

	// Check the surface capabilities and formats
	vk::SurfaceCapabilitiesKHR surfCapabilities;
	err1 = demo->fpGetPhysicalDeviceSurfaceCapabilitiesKHR(
		reinterpret_cast<VkPhysicalDevice &>(demo->gpu),
		reinterpret_cast<VkSurfaceKHR &>(demo->surface),
		&reinterpret_cast<VkSurfaceCapabilitiesKHR &>(surfCapabilities)
		);

	assert(!err1);

	uint32_t presentModeCount;

	err1 = demo->fpGetPhysicalDeviceSurfacePresentModesKHR(
		reinterpret_cast<VkPhysicalDevice &>(demo->gpu),
		reinterpret_cast<VkSurfaceKHR &>(demo->surface),
		&presentModeCount, NULL);

	assert(!err1);

	VkPresentModeKHR *presentModes = (VkPresentModeKHR *)malloc(presentModeCount * sizeof(VkPresentModeKHR));
	assert(presentModes);
	err1 = demo->fpGetPhysicalDeviceSurfacePresentModesKHR(
		reinterpret_cast<VkPhysicalDevice &>(demo->gpu),
		reinterpret_cast<VkSurfaceKHR &>(demo->surface),
		&presentModeCount, presentModes);

	//err = demo->gpu.getSurfacePresentModesKHR(demo->surface, &presentModeCount, presentModes);
	assert(!err1);

	vk::Extent2D swapchainExtent;
	// width and height are either both -1, or both not -1.
	if (surfCapabilities.currentExtent.width == (uint32_t)-1) {
		// If the surface size is undefined, the size is set to
		// the size of the images requested.
		swapchainExtent.setWidth(demo->width);
		swapchainExtent.setHeight(demo->height);
	}
	else {
		// If the surface size is defined, the swap chain size must match
		swapchainExtent = surfCapabilities.currentExtent;
		demo->width = surfCapabilities.currentExtent.width;
		demo->height = surfCapabilities.currentExtent.height;
	}

	vk::PresentModeKHR swapchainPresentMode = vk::PresentModeKHR::eFifo;

	// Determine the number of VkImage's to use in the swap chain (we desire to
	// own only 1 image at a time, besides the images being displayed and
	// queued for display):
	uint32_t desiredNumberOfSwapchainImages =
		surfCapabilities.minImageCount + 1;
	if ((surfCapabilities.maxImageCount > 0) &&
		(desiredNumberOfSwapchainImages > surfCapabilities.maxImageCount)) {
		// Application must settle for fewer images than desired:
		desiredNumberOfSwapchainImages = surfCapabilities.maxImageCount;
	}

	vk::SurfaceTransformFlagBitsKHR preTransform;
	if (surfCapabilities.supportedTransforms &
		vk::SurfaceTransformFlagBitsKHR::eIdentity) {
		preTransform = vk::SurfaceTransformFlagBitsKHR::eIdentity;
	}
	else {
		preTransform = surfCapabilities.currentTransform;
	}

	const vk::SwapchainCreateInfoKHR swapchain = vk::SwapchainCreateInfoKHR(
		vk::SwapchainCreateFlagBitsKHR(),									// flags
		demo->surface,														// surface
		desiredNumberOfSwapchainImages,										// minImageCount
		demo->format,														// imageFormat
		demo->color_space,													// imageColorSpace
		vk::Extent2D(swapchainExtent.width, swapchainExtent.height),	// imageExtent
		1,																	// imageArrayLayers
		vk::ImageUsageFlagBits::eColorAttachment,							// imageUsage
		vk::SharingMode::eExclusive,										// imageSharingMode
		0,																	// queueFamilyIndexCount
		NULL,															// pQueueFamilyIndicies
		preTransform,														// preTransform
		vk::CompositeAlphaFlagBitsKHR::eOpaque,								// compositeAlpha
		swapchainPresentMode,												// presentMode
		VK_TRUE,															// clipped
		oldSwapchain														// oldSwapChain
		);

	uint32_t i;

	err1 = demo->fpCreateSwapchainKHR(
		reinterpret_cast<VkDevice &>(demo->device),
		&reinterpret_cast<const VkSwapchainCreateInfoKHR &>(swapchain),
		NULL,
		&reinterpret_cast<VkSwapchainKHR &>(demo->swapchain)
		);

	assert(!err1);

	// If we just re-created an existing swapchain, we should destroy the old
	// swapchain at this point.
	// Note: destroying the swapchain also cleans up all its associated
	// presentable images once the platform is done with them.
	if (reinterpret_cast<bool &>(oldSwapchain)) {
		//demo->device.destroySwapchainKHR(oldSwapchain, NULL);
		demo->fpDestroySwapchainKHR( reinterpret_cast<VkDevice &>(demo->device),
			reinterpret_cast<VkSwapchainKHR &>(oldSwapchain), NULL
		);
	}

	err1 = demo->fpGetSwapchainImagesKHR(
		reinterpret_cast<const VkDevice &>(demo->device),
		reinterpret_cast<VkSwapchainKHR &>(demo->swapchain), &demo->swapchainImageCount, NULL );
	assert(!err1);

	VkImage *swapchainImages = (VkImage *)malloc(demo->swapchainImageCount * sizeof(VkImage));

	assert(swapchainImages);
	err1 = demo->fpGetSwapchainImagesKHR(
		reinterpret_cast<const VkDevice &>(demo->device),
		reinterpret_cast<VkSwapchainKHR &>(demo->swapchain),
		&demo->swapchainImageCount,
		swapchainImages
		);

	assert(!err1);

	demo->buffers = (SwapchainBuffers *)malloc(sizeof(SwapchainBuffers) *
		demo->swapchainImageCount);
	assert(demo->buffers);

	for (i = 0; i < demo->swapchainImageCount; i++) {
		vk::ImageViewCreateInfo color_attachment_view(
			vk::ImageViewCreateFlags(),
			vk::Image(),
			vk::ImageViewType::e2D,
			demo->format,
			vk::ComponentMapping(
				vk::ComponentSwizzle::eR,
				vk::ComponentSwizzle::eG,
				vk::ComponentSwizzle::eB,
				vk::ComponentSwizzle::eA
				),
			vk::ImageSubresourceRange
			(vk::ImageAspectFlagBits::eColor, 0, 1, 0, 1)
			);

		demo->buffers[i].image = vk::Image(swapchainImages[i]);

		// Render loop will expect image to have been used before and in
		// VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
		// layout and will change to COLOR_ATTACHMENT_OPTIMAL, so init the image
		// to that state
		demo_set_image_layout(
			demo, demo->buffers[i].image, vk::ImageAspectFlagBits::eColor,
			vk::ImageLayout::eUndefined, vk::ImageLayout::ePresentSrcKHR,
			vk::AccessFlagBits());

		color_attachment_view.setImage(demo->buffers[i].image);

		err = demo->device.createImageView(&color_attachment_view, NULL,
			&demo->buffers[i].view);
		assert(err == vk::Result::eSuccess);
	}

	demo->current_buffer = 0;

	if (NULL != presentModes) {
		free(presentModes);
	}
}

// Checked 6/8/2016
static void demo_prepare_depth(demoStruct* demo) {
	const vk::Format depth_format = vk::Format::eD16Unorm;
	const vk::ImageCreateInfo image(
		vk::ImageCreateFlags(),
		vk::ImageType::e2D,
		depth_format,
		vk::Extent3D(demo->width, demo->height, 1),
		1,
		1,
		vk::SampleCountFlagBits::e1,
		vk::ImageTiling::eOptimal,
		vk::ImageUsageFlagBits::eDepthStencilAttachment,
		vk::SharingMode::eExclusive,
		0,
		NULL,
		vk::ImageLayout::eUndefined
		);

	vk::MemoryAllocateInfo mem_alloc(0, 0);

	vk::ImageViewCreateInfo view(
		vk::ImageViewCreateFlags(),
		vk::Image(),
		vk::ImageViewType::e2D,
		depth_format,
		vk::ComponentMapping(),
		vk::ImageSubresourceRange(vk::ImageAspectFlagBits::eDepth, 0, 1, 0, 1)
		);

	vk::MemoryRequirements mem_reqs;
	vk::Result U_ASSERT_ONLY err;
	bool U_ASSERT_ONLY pass;

	demo->depth.format = depth_format;

	/* create image */
	err = demo->device.createImage(&image, NULL, &demo->depth.image);
	assert(err == vk::Result::eSuccess);

	/* get memory requirements for this object */
	demo->device.getImageMemoryRequirements(demo->depth.image, &mem_reqs);

	/* select memory size and type */
	mem_alloc.setAllocationSize(mem_reqs.size);
	pass = memory_type_from_properties(demo, mem_reqs.memoryTypeBits,
		vk::MemoryPropertyFlags(), /* No requirements */
		&mem_alloc.memoryTypeIndex);
	assert(pass);

	/* allocate memory */
	err = demo->device.allocateMemory(&mem_alloc, NULL, &demo->depth.mem);
	assert(err == vk::Result::eSuccess);

	/* bind memory */
	err = demo->device.bindImageMemory(demo->depth.image, demo->depth.mem, 0);
	assert(err == vk::Result::eSuccess);

	demo_set_image_layout(demo, demo->depth.image, vk::ImageAspectFlagBits::eDepth,
		vk::ImageLayout::eUndefined,
		vk::ImageLayout::eDepthStencilAttachmentOptimal,
		vk::AccessFlagBits());

	/* create image view */
	view.setImage(demo->depth.image);
	err = demo->device.createImageView(&view, NULL, &demo->depth.view);
	assert(err == vk::Result::eSuccess);
}

// Checked 6/8/2016
static void demo_prepare_texture_image(demoStruct* demo, const uint32_t *tex_colors,
	struct texture_object *tex_obj, vk::ImageTiling tiling,
	vk::ImageUsageFlags usage, vk::MemoryPropertyFlags required_props) {
	const vk::Format tex_format = vk::Format::eB8G8R8A8Unorm;
	const int32_t tex_width = 2;
	const int32_t tex_height = 2;
	vk::Result U_ASSERT_ONLY err;
	bool U_ASSERT_ONLY pass;

	tex_obj->tex_width = tex_width;
	tex_obj->tex_height = tex_height;

	const vk::ImageCreateInfo image_create_info(
		vk::ImageCreateFlags(),
		vk::ImageType::e2D,
		tex_format,
		vk::Extent3D(tex_width, tex_height, 1),
		1, 1,
		vk::SampleCountFlagBits::e1,
		tiling,
		usage,
		vk::SharingMode::eExclusive,
		0,
		NULL,
		vk::ImageLayout::ePreinitialized
		);

	vk::MemoryAllocateInfo mem_alloc(0, 0);

	vk::MemoryRequirements mem_reqs;

	err = demo->device.createImage(&image_create_info, NULL, &tex_obj->image);
	assert(err == vk::Result::eSuccess);

	demo->device.getImageMemoryRequirements(tex_obj->image, &mem_reqs);

	mem_alloc.setAllocationSize(mem_reqs.size);
	pass =
		memory_type_from_properties(demo, mem_reqs.memoryTypeBits,
			required_props, &mem_alloc.memoryTypeIndex);
	assert(pass);

	/* allocate memory */
	err = demo->device.allocateMemory(&mem_alloc, NULL, &tex_obj->mem);
	assert(err == vk::Result::eSuccess);

	/* bind memory */
	err = demo->device.bindImageMemory(tex_obj->image, tex_obj->mem, 0);
	assert(err == vk::Result::eSuccess);

	if (required_props & vk::MemoryPropertyFlagBits::eHostVisible) {
		const vk::ImageSubresource subres
			(vk::ImageAspectFlagBits::eColor, 0, 0);

		vk::SubresourceLayout layout;
		void *data;
		int32_t x, y;

		demo->device.getImageSubresourceLayout(tex_obj->image, &subres, &layout);

		err = demo->device.mapMemory(tex_obj->mem, 0,
			mem_alloc.allocationSize, vk::MemoryMapFlags(), &data);
		assert(err == vk::Result::eSuccess);

		for (y = 0; y < tex_height; y++) {
			uint32_t *row = (uint32_t *)((char *)data + layout.rowPitch * y);
			for (x = 0; x < tex_width; x++)
				row[x] = tex_colors[(x & 1) ^ (y & 1)];
		}

		demo->device.unmapMemory(tex_obj->mem);
	}

	tex_obj->imageLayout = vk::ImageLayout::eShaderReadOnlyOptimal;
	demo_set_image_layout(demo, tex_obj->image, vk::ImageAspectFlagBits::eColor,
		vk::ImageLayout::ePreinitialized, tex_obj->imageLayout,
		vk::AccessFlagBits::eHostWrite);
	/* setting the image layout does not reference the actual memory so no need
	* to add a mem ref */
}

// Checked 6/8/16
static void demo_destroy_texture_image(demoStruct* demo,
struct texture_object *tex_obj) {
	/* clean up staging resources */
	demo->device.destroyImage(tex_obj->image, NULL);
	demo->device.freeMemory(tex_obj->mem, NULL);
}

// Checked 6/8/16
static void demo_prepare_textures(demoStruct* demo) {
	const vk::Format tex_format = vk::Format::eB8G8R8A8Unorm;
	vk::FormatProperties props;
	const uint32_t tex_colors[DEMO_TEXTURE_COUNT][2] = {
		{ 0xffff0000, 0xff00ff00 },
	};
	uint32_t i;
	vk::Result U_ASSERT_ONLY err;

	demo->gpu.getFormatProperties(tex_format, &props);

	for (i = 0; i < DEMO_TEXTURE_COUNT; i++) {
		if ((props.linearTilingFeatures & vk::FormatFeatureFlagBits::eSampledImage) && !demo->use_staging_buffer) {
			/* Device can texture using linear textures */
			demo_prepare_texture_image(demo, tex_colors[i], &demo->textures[i],
				vk::ImageTiling::eLinear,
				vk::ImageUsageFlagBits::eSampled,
				vk::MemoryPropertyFlagBits::eHostVisible
				);
		}
		else if (props.optimalTilingFeatures & vk::FormatFeatureFlagBits::eSampledImage) {
			/* Must use staging buffer to copy linear texture to optimized */
			struct texture_object staging_texture;

			memset(&staging_texture, 0, sizeof(staging_texture));
			demo_prepare_texture_image(
				demo, tex_colors[i], &staging_texture,
				vk::ImageTiling::eLinear,
				vk::ImageUsageFlagBits::eTransferSrc,
				vk::MemoryPropertyFlagBits::eHostVisible
				);

			demo_prepare_texture_image(
				demo, tex_colors[i], &demo->textures[i],
				vk::ImageTiling::eOptimal,
				(vk::ImageUsageFlagBits::eTransferDst | vk::ImageUsageFlagBits::eSampled),
				vk::MemoryPropertyFlagBits::eDeviceLocal
				);

			demo_set_image_layout(
				demo,
				staging_texture.image,
				vk::ImageAspectFlagBits::eColor,
				staging_texture.imageLayout,
				vk::ImageLayout::eTransferSrcOptimal,
				vk::AccessFlagBits()
				);

			demo_set_image_layout(
				demo,
				demo->textures[i].image,
				vk::ImageAspectFlagBits::eColor,
				demo->textures[i].imageLayout,
				vk::ImageLayout::eTransferDstOptimal,
				vk::AccessFlagBits()
				);

			vk::ImageCopy copy_region(
				vk::ImageSubresourceLayers(vk::ImageAspectFlagBits::eColor, 0, 0, 1),
				vk::Offset3D(0, 0, 0),
				vk::ImageSubresourceLayers(vk::ImageAspectFlagBits::eColor, 0, 0, 1),
				vk::Offset3D(0, 0, 0),
				vk::Extent3D(staging_texture.tex_width, staging_texture.tex_height, 1)
				);

			demo->setup_cmd.copyImage(staging_texture.image,
				vk::ImageLayout::eTransferSrcOptimal, demo->textures[i].image,
				vk::ImageLayout::eTransferDstOptimal, 1, &copy_region
				);

			demo_set_image_layout(demo, demo->textures[i].image,
				vk::ImageAspectFlagBits::eColor,
				vk::ImageLayout::eTransferDstOptimal,
				demo->textures[i].imageLayout,
				vk::AccessFlagBits()
				);

			demo_flush_init_cmd(demo);

			demo_destroy_texture_image(demo, &staging_texture);
		}
		else {
			/* Can't support VK_FORMAT_B8G8R8A8_UNORM !? */
			assert(!"No support for B8G8R8A8_UNORM as texture image format");
		}

		const vk::SamplerCreateInfo sampler(
			vk::SamplerCreateFlags(),
			vk::Filter::eNearest,
			vk::Filter::eNearest,
			vk::SamplerMipmapMode::eNearest,
			vk::SamplerAddressMode::eRepeat,
			vk::SamplerAddressMode::eRepeat,
			vk::SamplerAddressMode::eRepeat, 0.f, VK_FALSE, 1, 0,
			vk::CompareOp::eNever, 0, 0,
			vk::BorderColor::eFloatOpaqueWhite, VK_FALSE
			);

		vk::ImageViewCreateInfo view(
			vk::ImageViewCreateFlags(),
			VK_NULL_HANDLE,
			vk::ImageViewType::e2D,
			tex_format,
			vk::ComponentMapping(
				vk::ComponentSwizzle::eR,
				vk::ComponentSwizzle::eG,
				vk::ComponentSwizzle::eB,
				vk::ComponentSwizzle::eA
				),
			vk::ImageSubresourceRange(vk::ImageAspectFlagBits::eColor, 0, 1, 0, 1)
			);

		/* create sampler */
		err = demo->device.createSampler(
			&sampler, NULL,
			&demo->textures[i].sampler
			);
		assert(err == vk::Result::eSuccess);

		/* create image view */
		view.setImage(demo->textures[i].image);

		err = demo->device.createImageView(
			&view, NULL,
			&demo->textures[i].view
			);
		assert(err == vk::Result::eSuccess);
	}
}

//Checked 6/8/16
static void demo_prepare_vertices(demoStruct* demo) {
	// clang-format off
	const float vb[3][5] = {
		/*      position             texcoord */
		{ -1.0f, -1.0f,  0.25f,     0.0f, 0.0f },
		{ 1.0f, -1.0f,  0.25f,     1.0f, 0.0f },
		{ 0.0f,  1.0f,  3.0f,      0.5f, 1.0f },
	};

	const vk::BufferCreateInfo buf_info(
		vk::BufferCreateFlags(),
		sizeof(vb),
		vk::BufferUsageFlagBits::eVertexBuffer,
		vk::SharingMode::eExclusive,
		0,
		NULL
		);

	vk::MemoryAllocateInfo mem_alloc(0, 0);

	vk::MemoryRequirements mem_reqs;
	vk::Result U_ASSERT_ONLY err;
	bool U_ASSERT_ONLY pass;
	void *data;

	memset(&demo->vertices, 0, sizeof(demo->vertices));

	err = demo->device.createBuffer(&buf_info, NULL, &demo->vertices.buf);
	assert(err == vk::Result::eSuccess);

	/////
	demo->device.getBufferMemoryRequirements(demo->vertices.buf, &mem_reqs);
	assert(err == vk::Result::eSuccess);

	mem_alloc.setAllocationSize(mem_reqs.size);
	pass = memory_type_from_properties(
		demo,
		mem_reqs.memoryTypeBits,
		vk::MemoryPropertyFlagBits::eHostVisible,
		&mem_alloc.memoryTypeIndex);
	assert(pass);
	////

	err = demo->device.allocateMemory(&mem_alloc, NULL, &demo->vertices.mem);
	assert(err == vk::Result::eSuccess);

	err = demo->device.mapMemory(
		demo->vertices.mem, 0,
		mem_alloc.allocationSize,
		vk::MemoryMapFlags(),
		&data
		);
	assert(err == vk::Result::eSuccess);

	memcpy(data, vb, sizeof(vb));

	err = demo->device.bindBufferMemory(
		demo->vertices.buf,
		demo->vertices.mem, 0
		);
	assert(err == vk::Result::eSuccess);

	demo->device.unmapMemory(demo->vertices.mem);

	demo->vertices.vi.setSType(vk::StructureType::ePipelineVertexInputStateCreateInfo);
	demo->vertices.vi.setPNext(NULL);
	demo->vertices.vi.setVertexBindingDescriptionCount(1);
	demo->vertices.vi.setPVertexBindingDescriptions(demo->vertices.vi_bindings);
	demo->vertices.vi.setVertexAttributeDescriptionCount(2);
	demo->vertices.vi.setPVertexAttributeDescriptions(demo->vertices.vi_attrs);

	demo->vertices.vi_bindings[0].setBinding(VERTEX_BUFFER_BIND_ID);
	demo->vertices.vi_bindings[0].setStride(sizeof(vb[0]));
	demo->vertices.vi_bindings[0].setInputRate(vk::VertexInputRate::eVertex);
	 
	demo->vertices.vi_attrs[0].setBinding(VERTEX_BUFFER_BIND_ID);
	demo->vertices.vi_attrs[0].setLocation(0);
	demo->vertices.vi_attrs[0].setFormat(vk::Format::eR32G32B32Sfloat);
	demo->vertices.vi_attrs[0].setOffset(0);

	demo->vertices.vi_attrs[1].setBinding(VERTEX_BUFFER_BIND_ID);
	demo->vertices.vi_attrs[1].setLocation(1);
	demo->vertices.vi_attrs[1].setFormat(vk::Format::eR32G32Sfloat);
	demo->vertices.vi_attrs[1].setOffset(sizeof(float) * 3);
}

//Checked 6/8/16
static void demo_prepare_descriptor_layout(demoStruct* demo) {
	const vk::DescriptorSetLayoutBinding layout_binding(
		0,
		vk::DescriptorType::eCombinedImageSampler,
		DEMO_TEXTURE_COUNT,
		vk::ShaderStageFlagBits::eFragment,
		NULL
		);

	const vk::DescriptorSetLayoutCreateInfo descriptor_layout(
		vk::DescriptorSetLayoutCreateFlags(),
		1,
		&layout_binding
		);

	vk::Result U_ASSERT_ONLY err;

	err = demo->device.createDescriptorSetLayout(
		&descriptor_layout,
		NULL,
		&demo->desc_layout
		);

	assert(err == vk::Result::eSuccess);

	const vk::PipelineLayoutCreateInfo pPipelineLayoutCreateInfo(
		vk::PipelineLayoutCreateFlags(),
		1,
		&demo->desc_layout,
		0,
		NULL
		);

	err = demo->device.createPipelineLayout(
		&pPipelineLayoutCreateInfo,
		NULL,
		&demo->pipeline_layout
		);
	assert(err == vk::Result::eSuccess);
}

//Checked 6/8/16
static void demo_prepare_render_pass(demoStruct* demo) {
	const vk::AttachmentDescription attachments[2] = {
		vk::AttachmentDescription(
		vk::AttachmentDescriptionFlags(),
			demo->format,
			vk::SampleCountFlagBits::e1,
			vk::AttachmentLoadOp::eClear,
			vk::AttachmentStoreOp::eStore,
			vk::AttachmentLoadOp::eDontCare,
			vk::AttachmentStoreOp::eDontCare,
			vk::ImageLayout::eColorAttachmentOptimal,
			vk::ImageLayout::eColorAttachmentOptimal
			),
		vk::AttachmentDescription(
			vk::AttachmentDescriptionFlags(),
			demo->depth.format,
			vk::SampleCountFlagBits::e1,
			vk::AttachmentLoadOp::eClear,
			vk::AttachmentStoreOp::eDontCare,
			vk::AttachmentLoadOp::eDontCare,
			vk::AttachmentStoreOp::eDontCare,
			vk::ImageLayout::eDepthStencilAttachmentOptimal,
			vk::ImageLayout::eDepthStencilAttachmentOptimal
			),
	};

	const vk::AttachmentReference color_reference
		(0, vk::ImageLayout::eColorAttachmentOptimal);

	const vk::AttachmentReference depth_reference
		(1, vk::ImageLayout::eDepthStencilAttachmentOptimal);

	const vk::SubpassDescription subpass(
		vk::SubpassDescriptionFlags(),
		vk::PipelineBindPoint::eGraphics,
		0,
		VK_NULL_HANDLE,
		1,
		&color_reference,
		VK_NULL_HANDLE,
		&depth_reference,
		0,
		VK_NULL_HANDLE
		);

	const vk::RenderPassCreateInfo rp_info(
		vk::RenderPassCreateFlags(),
		2,
		attachments,
		1,
		&subpass,
		0,
		VK_NULL_HANDLE
		);

	vk::Result U_ASSERT_ONLY err;

	err = demo->device.createRenderPass(&rp_info, NULL, &demo->render_pass);

	assert(err == vk::Result::eSuccess);
}

//Checked 6/8/16
static vk::ShaderModule demo_prepare_shader_module(demoStruct* demo, const void *code, size_t size) {
	vk::ShaderModuleCreateInfo moduleCreateInfo(
		vk::ShaderModuleCreateFlags(),
		size,
		(uint32_t*)code
		);

	vk::ShaderModule module;
	vk::Result U_ASSERT_ONLY err;

	err = demo->device.createShaderModule(&moduleCreateInfo, NULL, &module);
	assert(err == vk::Result::eSuccess);

	return module;
}

//Checked 6/8/16
char *demo_read_spv(const char *filename, size_t *psize) {
	long int size;
	void *shader_code;
	size_t retVal;

	FILE *fp = fopen(filename, "rb");
	if (!fp)
		return NULL;

	fseek(fp, 0L, SEEK_END);
	size = ftell(fp);

	fseek(fp, 0L, SEEK_SET);

	shader_code = malloc(size);
	retVal = fread(shader_code, size, 1, fp);
	if (!retVal)
		return NULL;

	*psize = size;

	fclose(fp);
	return (char*)shader_code;
}

//Checked 6/8/16
static vk::ShaderModule demo_prepare_vs(demoStruct* demo) {
	void *vertShaderCode;
	size_t size;

	vertShaderCode = demo_read_spv("tri-vert.spv", &size);

	demo->vert_shader_module =
		demo_prepare_shader_module(demo, vertShaderCode, size);

	free(vertShaderCode);

	return demo->vert_shader_module;
}

//Checked 6/8/16
static vk::ShaderModule demo_prepare_fs(demoStruct* demo) {
	void *fragShaderCode;
	size_t size;

	fragShaderCode = demo_read_spv("tri-frag.spv", &size);

	demo->frag_shader_module =
		demo_prepare_shader_module(demo, fragShaderCode, size);

	free(fragShaderCode);

	return demo->frag_shader_module;
}

//Checked 6/7/2016
static void demo_prepare_pipeline(demoStruct* demo) {
	vk::GraphicsPipelineCreateInfo pipeline = vk::GraphicsPipelineCreateInfo();
	vk::PipelineCacheCreateInfo pipelineCache = vk::PipelineCacheCreateInfo();

	vk::PipelineVertexInputStateCreateInfo vi = vk::PipelineVertexInputStateCreateInfo();
	vk::PipelineInputAssemblyStateCreateInfo ia = vk::PipelineInputAssemblyStateCreateInfo();
	vk::PipelineRasterizationStateCreateInfo rs = vk::PipelineRasterizationStateCreateInfo();
	vk::PipelineColorBlendStateCreateInfo cb = vk::PipelineColorBlendStateCreateInfo();
	vk::PipelineDepthStencilStateCreateInfo ds = vk::PipelineDepthStencilStateCreateInfo();
	vk::PipelineViewportStateCreateInfo vp = vk::PipelineViewportStateCreateInfo();
	vk::PipelineMultisampleStateCreateInfo ms = vk::PipelineMultisampleStateCreateInfo();
	vk::DynamicState dynamicStateEnables[VK_DYNAMIC_STATE_RANGE_SIZE];
	vk::PipelineDynamicStateCreateInfo dynamicState = vk::PipelineDynamicStateCreateInfo();

	vk::Result U_ASSERT_ONLY err;

	memset(dynamicStateEnables, 0, sizeof( dynamicStateEnables));
	memset(&dynamicState, 0, sizeof( dynamicState));
	dynamicState.setSType(vk::StructureType::ePipelineDynamicStateCreateInfo);
	dynamicState.setPDynamicStates(dynamicStateEnables);

	memset(&pipeline, 0, sizeof(pipeline));
	pipeline.setSType(vk::StructureType::eGraphicsPipelineCreateInfo);
	pipeline.setLayout(demo->pipeline_layout);

	vi = demo->vertices.vi;

	memset(&ia, 0, sizeof(ia));
	ia.setSType(vk::StructureType::ePipelineInputAssemblyStateCreateInfo);
	ia.setTopology(vk::PrimitiveTopology::eTriangleList);

	memset(&rs, 0, sizeof(rs));
	rs.setSType(vk::StructureType::ePipelineRasterizationStateCreateInfo);
	rs.setPolygonMode(vk::PolygonMode::eLine);
	rs.setCullMode(vk::CullModeFlagBits::eBack);
	rs.setFrontFace(vk::FrontFace::eClockwise);
	rs.setDepthClampEnable(VK_FALSE);
	rs.setRasterizerDiscardEnable(VK_FALSE);
	rs.setDepthBiasEnable(VK_FALSE);

	memset(&cb, 0, sizeof(cb));
	cb.setSType(vk::StructureType::ePipelineColorBlendStateCreateInfo);
	vk::PipelineColorBlendAttachmentState att_state[1];
	memset(&att_state, 0, sizeof(att_state));
	att_state[0] = {
		vk::PipelineColorBlendAttachmentState
		(VK_FALSE,
			vk::BlendFactor::eOne,
			vk::BlendFactor::eZero,
			vk::BlendOp::eAdd,
			vk::BlendFactor::eOne,
			vk::BlendFactor::eZero,
			vk::BlendOp::eAdd,
			vk::ColorComponentFlags()
			)
	};
	cb.setAttachmentCount(1);
	cb.setPAttachments(att_state);

	memset(&vp, 0, sizeof(vp));
	vp.setSType(vk::StructureType::ePipelineViewportStateCreateInfo);
	vp.setViewportCount(1);

	dynamicStateEnables[dynamicState.dynamicStateCount++] = (vk::DynamicState::eViewport);
	vp.setScissorCount(1);
	dynamicStateEnables[dynamicState.dynamicStateCount++] = (vk::DynamicState::eScissor);

	memset(&ds, 0, sizeof(ds));
	ds.setSType(vk::StructureType::ePipelineDepthStencilStateCreateInfo);
	ds.setDepthTestEnable(VK_FALSE);
	ds.setDepthWriteEnable(VK_FALSE);
	ds.setDepthCompareOp(vk::CompareOp::eLessOrEqual);
	ds.setDepthBoundsTestEnable(VK_FALSE);
	ds.back.setFailOp(vk::StencilOp::eKeep);
	ds.back.setPassOp(vk::StencilOp::eKeep);
	ds.back.setCompareOp(vk::CompareOp::eAlways);
	ds.setStencilTestEnable(VK_FALSE);
	ds.setFront(ds.back);

	memset(&ms, 0, sizeof(ms));
	ms.setSType(vk::StructureType::ePipelineMultisampleStateCreateInfo);
	ms.setPSampleMask(NULL);
	ms.setRasterizationSamples(vk::SampleCountFlagBits::e1);

	// Two stages: vs and fs
	pipeline.setStageCount(2);

	vk::PipelineShaderStageCreateInfo shaderStages[2] = {
		vk::PipelineShaderStageCreateInfo(
		vk::PipelineShaderStageCreateFlags(),
			vk::ShaderStageFlagBits::eVertex,
			demo_prepare_vs(demo),
			"main", nullptr
			),
		vk::PipelineShaderStageCreateInfo(
			vk::PipelineShaderStageCreateFlags(),
			vk::ShaderStageFlagBits::eFragment,
			demo_prepare_fs(demo),
			"main", nullptr
			)
	};

	pipeline.setPVertexInputState(&vi);
	pipeline.setPInputAssemblyState(&ia);
	pipeline.setPRasterizationState(&rs);
	pipeline.setPColorBlendState(&cb);
	pipeline.setPMultisampleState(&ms);
	pipeline.setPViewportState(&vp);
	pipeline.setPDepthStencilState(&ds);
	pipeline.setPStages(shaderStages);
	pipeline.setRenderPass(demo->render_pass);
	pipeline.setPDynamicState(&dynamicState);

	memset(&pipelineCache, 0, sizeof(pipelineCache));
	pipelineCache.setSType(vk::StructureType::ePipelineCacheCreateInfo);

	err = demo->device.createPipelineCache(
		&pipelineCache, NULL,
		&demo->pipelineCache
		);

	assert(err == vk::Result::eSuccess);

	err = demo->device.createGraphicsPipelines(
		demo->pipelineCache,
		1,
		&pipeline, NULL,
		&demo->pipeline
		);

	assert(err == vk::Result::eSuccess);

	demo->device.destroyPipelineCache(demo->pipelineCache, NULL);
	demo->device.destroyShaderModule(demo->frag_shader_module, NULL);
	demo->device.destroyShaderModule(demo->vert_shader_module, NULL);
}

//Checked 6/7/16
static void demo_prepare_descriptor_pool(demoStruct* demo) {
	const vk::DescriptorPoolSize type_count
		(vk::DescriptorType::eCombinedImageSampler, DEMO_TEXTURE_COUNT);

	const vk::DescriptorPoolCreateInfo descriptor_pool
		(vk::DescriptorPoolCreateFlags(), 1, 1, &type_count);

	vk::Result U_ASSERT_ONLY err;

	err = demo->device.createDescriptorPool(&descriptor_pool, NULL,
		&demo->desc_pool);
	assert(err == vk::Result::eSuccess);
}

//Checked 6/7/16
static void demo_prepare_descriptor_set(demoStruct* demo) {
	vk::DescriptorImageInfo tex_descs[DEMO_TEXTURE_COUNT];
	vk::WriteDescriptorSet write;
	vk::Result U_ASSERT_ONLY err;
	uint32_t i;

	vk::DescriptorSetAllocateInfo alloc_info
		(
			demo->desc_pool,
			1,
			&demo->desc_layout
			);
	err = demo->device.allocateDescriptorSets(&alloc_info, &demo->desc_set);
	assert(err == vk::Result::eSuccess);

	memset(&tex_descs, 0, sizeof(tex_descs));
	for (i = 0; i < DEMO_TEXTURE_COUNT; i++) {
		tex_descs[i].setSampler(demo->textures[i].sampler);
		tex_descs[i].setImageView(demo->textures[i].view);
		tex_descs[i].setImageLayout(vk::ImageLayout::eGeneral);
	}

	memset(&write, 0, sizeof(write));
	write.setSType(vk::StructureType::eWriteDescriptorSet);
	write.setDstSet(demo->desc_set);
	write.setDescriptorCount(DEMO_TEXTURE_COUNT);
	write.setDescriptorType(vk::DescriptorType::eCombinedImageSampler);
	write.setPImageInfo(tex_descs);

	demo->device.updateDescriptorSets(1, &write, 0, NULL);
}

//Checked 6/7/16
static void demo_prepare_framebuffers(demoStruct* demo) {
	vk::ImageView attachments[2];
	attachments[1] = demo->depth.view;

	const vk::FramebufferCreateInfo fb_info
		(
			vk::FramebufferCreateFlags(),
			demo->render_pass,
			2,
			attachments,
			demo->width,
			demo->height,
			1
			);
	vk::Result U_ASSERT_ONLY err;
	uint32_t i;

	demo->framebuffers = (vk::Framebuffer *)malloc(demo->swapchainImageCount * sizeof(vk::Framebuffer));
	assert(demo->framebuffers);


	for (i = 0; i < demo->swapchainImageCount; i++) {
		attachments[0] = demo->buffers[i].view;
		err = demo->device.createFramebuffer(&fb_info, NULL,
			&demo->framebuffers[i]);

		assert(err == vk::Result::eSuccess);
	}
}
//############################################################################################################################################################################################################
//Checked 6/7/16
static void demo_prepare(demoStruct* demo) {
	vk::Result U_ASSERT_ONLY err;

	const vk::CommandPoolCreateInfo cmd_pool_info(
		vk::CommandPoolCreateFlagBits::eResetCommandBuffer,


		demo->graphics_queue_node_index
		);
	err = demo->device.createCommandPool(&cmd_pool_info, NULL,
		&demo->cmd_pool);
	assert(err == vk::Result::eSuccess);

	const vk::CommandBufferAllocateInfo cmd(


		demo->cmd_pool,
		vk::CommandBufferLevel::ePrimary,
		1
		);
	err = demo->device.allocateCommandBuffers(&cmd, &demo->draw_cmd);
	assert(err == vk::Result::eSuccess);

	demo_prepare_buffers(demo);
	demo_prepare_depth(demo);
	demo_prepare_textures(demo);
	demo_prepare_vertices(demo);
	demo_prepare_descriptor_layout(demo);
	demo_prepare_render_pass(demo);
	demo_prepare_pipeline(demo);

	demo_prepare_descriptor_pool(demo);
	demo_prepare_descriptor_set(demo);

	demo_prepare_framebuffers(demo);

	demo->prepared = true;
}

#ifdef _WIN32
static void demo_run(demoStruct* demo) {
	if (!demo->prepared)
		return;
	demo_draw(demo);

	/*if (demo->depthStencil > 0.99f)
	demo->depthIncrement = -0.001f;
	if (demo->depthStencil < 0.8f)
	demo->depthIncrement = 0.001f;

	demo->depthStencil += demo->depthIncrement;*/
}

// On MS-Windows, make this a global, so it's available to WndProc()
demoStruct demo;

// MS-Windows event handling function:
LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
	char tmp_str[] = APP_LONG_NAME;

	switch (uMsg) {
	case WM_CREATE:
		return 0;
	case WM_CLOSE:
		PostQuitMessage(0);
		return 0;
	case WM_PAINT:
		if (demo.prepared) {
			demo_run(&demo);
			break;
		}
	case WM_SIZE:
		// Resize the application to the new window size, except when
		// it was minimized. Vulkan doesn't support images or swapchains
		// with width=0 and height=0.
		if (wParam != SIZE_MINIMIZED) {
			demo.width = lParam & 0xffff;
			demo.height = lParam & 0xffff0000 >> 16;
			demo_resize(&demo);
		}
		break;
	default:
		break;
	}
	return (DefWindowProc(hWnd, uMsg, wParam, lParam));
}

static void demo_create_window(demoStruct* demo) {
	WNDCLASSEX win_class;

	// Initialize the window class structure:
	win_class.cbSize = sizeof(WNDCLASSEX);
	win_class.style = CS_HREDRAW | CS_VREDRAW;
	win_class.lpfnWndProc = WndProc;
	win_class.cbClsExtra = 0;
	win_class.cbWndExtra = 0;
	win_class.hInstance = demo->connection; // hInstance
	win_class.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	win_class.hCursor = LoadCursor(NULL, IDC_ARROW);
	win_class.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
	win_class.lpszMenuName = NULL;
	win_class.lpszClassName = demo->name;
	win_class.hIconSm = LoadIcon(NULL, IDI_WINLOGO);
	// Register window class:
	if (!RegisterClassEx(&win_class)) {
		// It didn't work, so try to give a useful error:
		printf("Unexpected error trying to start the application!\n");
		fflush(stdout);
		exit(1);
	}
	// Create window with the registered class:
	RECT wr = { 0, 0, demo->width, demo->height };
	AdjustWindowRect(&wr, WS_OVERLAPPEDWINDOW, FALSE);
	demo->window = CreateWindowEx(0,
		demo->name,           // class name
		demo->name,           // app name
		WS_OVERLAPPEDWINDOW | // window style
		WS_VISIBLE | WS_SYSMENU,
		100, 100,           // x/y coords
		wr.right - wr.left, // width
		wr.bottom - wr.top, // height
		NULL,               // handle to parent
		NULL,               // handle to menu
		demo->connection,   // hInstance
		NULL);              // no extra parameters
	if (!demo->window) {
		// It didn't work, so try to give a useful error:
		printf("Cannot create a window in which to draw!\n");
		fflush(stdout);
		exit(1);
	}
}
#else  // _WIN32

static void demo_handle_event(demoStruct* demo,
	const xcb_generic_event_t *event) {
	switch (event->response_type & 0x7f) {
	case XCB_EXPOSE:
		demo_draw(demo);
		break;
	case XCB_CLIENT_MESSAGE:
		if ((*(xcb_client_message_event_t *)event).data.data32[0] ==
			(*demo->atom_wm_delete_window).atom) {
			demo->quit = true;
		}
		break;
	case XCB_KEY_RELEASE: {
		const xcb_key_release_event_t *key =
			(const xcb_key_release_event_t *)event;

		if (key->detail == 0x9)
			demo->quit = true;
	} break;
	case XCB_DESTROY_NOTIFY:
		demo->quit = true;
		break;
	case XCB_CONFIGURE_NOTIFY: {
		const xcb_configure_notify_event_t *cfg =
			(const xcb_configure_notify_event_t *)event;
		if ((demo->width != cfg->width) || (demo->height != cfg->height)) {
			demo->width = cfg->width;
			demo->height = cfg->height;
			demo_resize(demo);
		}
	} break;
	default:
		break;
	}
}

static void demo_run(demoStruct* demo) {
	xcb_flush(demo->connection);

	while (!demo->quit) {
		xcb_generic_event_t *event;

		event = xcb_poll_for_event(demo->connection);
		if (event) {
			demo_handle_event(demo, event);
			free(event);
		}

		demo_draw(demo);

		if (demo->depthStencil > 0.99f)
			demo->depthIncrement = -0.001f;
		if (demo->depthStencil < 0.8f)
			demo->depthIncrement = 0.001f;

		demo->depthStencil += demo->depthIncrement;

		// Wait for work to finish before updating MVP.
		vkDeviceWaitIdle(demo->device);
	}
}

static void demo_create_window(demoStruct* demo) {
	uint32_t value_mask, value_list[32];

	demo->window = xcb_generate_id(demo->connection);

	value_mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
	value_list[0] = demo->screen->black_pixel;
	value_list[1] = XCB_EVENT_MASK_KEY_RELEASE | XCB_EVENT_MASK_EXPOSURE |
		XCB_EVENT_MASK_STRUCTURE_NOTIFY;

	xcb_create_window(demo->connection, XCB_COPY_FROM_PARENT, demo->window,
		demo->screen->root, 0, 0, demo->width, demo->height, 0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT, demo->screen->root_visual,
		value_mask, value_list);

	/* Magic code that will send notification when window is destroyed */
	xcb_intern_atom_cookie_t cookie =
		xcb_intern_atom(demo->connection, 1, 12, "WM_PROTOCOLS");
	xcb_intern_atom_reply_t *reply =
		xcb_intern_atom_reply(demo->connection, cookie, 0);

	xcb_intern_atom_cookie_t cookie2 =
		xcb_intern_atom(demo->connection, 0, 16, "WM_DELETE_WINDOW");
	demo->atom_wm_delete_window =
		xcb_intern_atom_reply(demo->connection, cookie2, 0);

	xcb_change_property(demo->connection, XCB_PROP_MODE_REPLACE, demo->window,
		(*reply).atom, 4, 32, 1,
		&(*demo->atom_wm_delete_window).atom);
	free(reply);

	xcb_map_window(demo->connection, demo->window);
}
#endif // _WIN32

/*
* Return 1 (true) if all layer names specified in check_names
* can be found in given layer properties.
*/
static vk::Bool32 demo_check_layers(
	uint32_t check_count, char **check_names,
	uint32_t layer_count, vk::LayerProperties *layers) {
	for (uint32_t i = 0; i < check_count; i++) {
		vk::Bool32 found = 0;
		for (uint32_t j = 0; j < layer_count; j++) {
			if (!strcmp(check_names[i], layers[j].layerName)) {
				found = 1;
				break;
			}
		}
		if (!found) {
			fprintf(stdout, "Cannot find layer: %s\n", check_names[i]);
			return 0;
		}
	}
	return 1;
}

//Checked 6/7/16
static void demo_init_vk(demoStruct* demo) {
	vk::Result err;
	uint32_t instance_extension_count = 0;
	uint32_t instance_layer_count = 0;
	uint32_t device_validation_layer_count = 0;
	char **instance_validation_layers = NULL;
	demo->enabled_extension_count = 0;
	demo->enabled_layer_count = 0;

	char *instance_validation_layers_alt1[] = {
		"VK_LAYER_LUNARG_standard_validation"
	};

	char *instance_validation_layers_alt2[] = {
		"VK_LAYER_GOOGLE_threading",     "VK_LAYER_LUNARG_param_checker",
		"VK_LAYER_LUNARG_device_limits", "VK_LAYER_LUNARG_object_tracker",
		"VK_LAYER_LUNARG_image",         "VK_LAYER_LUNARG_mem_tracker",
		"VK_LAYER_LUNARG_draw_state",    "VK_LAYER_LUNARG_swapchain",
		"VK_LAYER_GOOGLE_unique_objects"
	};

	/* Look for validation layers */
	vk::Bool32 validation_found = 0;
	if (demo->validate) {

		err = vk::enumerateInstanceLayerProperties(&instance_layer_count, NULL);
		assert(err == vk::Result::eSuccess);

		instance_validation_layers = instance_validation_layers_alt1;
		if (instance_layer_count > 0) {
			vk::LayerProperties *instance_layers =
				(vk::LayerProperties *)malloc(sizeof(vk::LayerProperties) * instance_layer_count);
			err = vk::enumerateInstanceLayerProperties(&instance_layer_count,
				instance_layers);
			assert(err == vk::Result::eSuccess);


			validation_found = demo_check_layers(
				ARRAY_SIZE(instance_validation_layers_alt1),
				instance_validation_layers, instance_layer_count,
				instance_layers);
			if (validation_found) {
				demo->enabled_layer_count = ARRAY_SIZE(instance_validation_layers_alt1);
				demo->device_validation_layers[0] = "VK_LAYER_LUNARG_standard_validation";
				device_validation_layer_count = 1;
			}
			else {

				instance_validation_layers = instance_validation_layers_alt2;
				demo->enabled_layer_count = ARRAY_SIZE(instance_validation_layers_alt2);
				validation_found = demo_check_layers(
					ARRAY_SIZE(instance_validation_layers_alt2),
					instance_validation_layers, instance_layer_count,
					instance_layers);
				device_validation_layer_count =
					ARRAY_SIZE(instance_validation_layers_alt2);
				for (uint32_t i = 0; i < device_validation_layer_count; i++) {
					demo->device_validation_layers[i] =
						instance_validation_layers[i];
				}
			}
			free(instance_layers);
		}

		if (!validation_found) {
			ERR_EXIT("vkEnumerateInstanceLayerProperties failed to find"
				"required validation layer.\n\n"
				"Please look at the Getting Started guide for additional "
				"information.\n",
				"vkCreateInstance Failure");
		}
	}

	/* Look for instance extensions */
	vk::Bool32 surfaceExtFound = 0;
	vk::Bool32 platformSurfaceExtFound = 0;
	memset(demo->extension_names, 0, sizeof(demo->extension_names));

	err = vk::enumerateInstanceExtensionProperties(
		NULL, &instance_extension_count, NULL);
	assert(err == vk::Result::eSuccess);

	if (instance_extension_count > 0) {
		vk::ExtensionProperties *instance_extensions =
			(vk::ExtensionProperties *)malloc(sizeof(vk::ExtensionProperties) * instance_extension_count);
		err = vk::enumerateInstanceExtensionProperties(
			NULL, &instance_extension_count, instance_extensions);
		assert(err == vk::Result::eSuccess);
		for (uint32_t i = 0; i < instance_extension_count; i++) {
			if (!strcmp(VK_KHR_SURFACE_EXTENSION_NAME,
				instance_extensions[i].extensionName)) {
				surfaceExtFound = 1;
				demo->extension_names[demo->enabled_extension_count++] =
					VK_KHR_SURFACE_EXTENSION_NAME;
			}
#ifdef _WIN32
			if (!strcmp(VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
				instance_extensions[i].extensionName)) {
				platformSurfaceExtFound = 1;
				demo->extension_names[demo->enabled_extension_count++] =
					VK_KHR_WIN32_SURFACE_EXTENSION_NAME;
			}
#else  // _WIN32
			if (!strcmp(VK_KHR_XCB_SURFACE_EXTENSION_NAME,
				instance_extensions[i].extensionName)) {
				platformSurfaceExtFound = 1;
				demo->extension_names[demo->enabled_extension_count++] =
					VK_KHR_XCB_SURFACE_EXTENSION_NAME;
			}
#endif // _WIN32
			if (!strcmp(VK_EXT_DEBUG_REPORT_EXTENSION_NAME,
				instance_extensions[i].extensionName)) {
				if (demo->validate) {
					demo->extension_names[demo->enabled_extension_count++] =
						VK_EXT_DEBUG_REPORT_EXTENSION_NAME;
				}
			}
			assert(demo->enabled_extension_count < 64);
		}

		free(instance_extensions);
	}

	if (!surfaceExtFound) {
		ERR_EXIT("vkEnumerateInstanceExtensionProperties failed to find "
			"the " VK_KHR_SURFACE_EXTENSION_NAME
			" extension.\n\nDo you have a compatible "
			"Vulkan installable client driver (ICD) installed?\nPlease "
			"look at the Getting Started guide for additional "
			"information.\n",
			"vkCreateInstance Failure");
	}
	if (!platformSurfaceExtFound) {
#ifdef _WIN32
		ERR_EXIT("vkEnumerateInstanceExtensionProperties failed to find "
			"the " VK_KHR_WIN32_SURFACE_EXTENSION_NAME
			" extension.\n\nDo you have a compatible "
			"Vulkan installable client driver (ICD) installed?\nPlease "
			"look at the Getting Started guide for additional "
			"information.\n",
			"vkCreateInstance Failure");
#else  // _WIN32
		ERR_EXIT("vkEnumerateInstanceExtensionProperties failed to find "
			"the " VK_KHR_XCB_SURFACE_EXTENSION_NAME
			" extension.\n\nDo you have a compatible "
			"Vulkan installable client driver (ICD) installed?\nPlease "
			"look at the Getting Started guide for additional "
			"information.\n",
			"vkCreateInstance Failure");
#endif // _WIN32
	}
	const vk::ApplicationInfo app(


		APP_SHORT_NAME,
		0,
		APP_SHORT_NAME,
		0,
		VK_MAKE_VERSION(1, 0, 3)
		);
	vk::InstanceCreateInfo inst_info(

		vk::InstanceCreateFlags(),
		&app,
		demo->enabled_layer_count,
		(const char *const *)instance_validation_layers,
		demo->enabled_extension_count,
		demo->extension_names
		);

	uint32_t gpu_count;

	err = vk::createInstance(&inst_info, NULL, &demo->inst);
	if (err == vk::Result::eErrorIncompatibleDriver) {
		ERR_EXIT("Cannot find a compatible Vulkan installable client driver "
			"(ICD).\n\nPlease look at the Getting Started guide for "
			"additional information.\n",
			"vkCreateInstance Failure");
	}
	else if (err == vk::Result::eErrorExtensionNotPresent) {
		ERR_EXIT("Cannot find a specified extension library"
			".\nMake sure your layers path is set appropriately\n",
			"vkCreateInstance Failure");
	}
	else if (err != vk::Result::eSuccess) {
		ERR_EXIT("vkCreateInstance failed.\n\nDo you have a compatible Vulkan "
			"installable client driver (ICD) installed?\nPlease look at "
			"the Getting Started guide for additional information.\n",
			"vkCreateInstance Failure");
	}

	/* Make initial call to query gpu_count, then second call for gpu info*/
	err = demo->inst.enumeratePhysicalDevices(&gpu_count, NULL);
	assert(err == vk::Result::eSuccess && gpu_count > 0);

	if (gpu_count > 0) {
		vk::PhysicalDevice *physical_devices =
			(vk::PhysicalDevice *)malloc(sizeof(vk::PhysicalDevice) * gpu_count);
		err = demo->inst.enumeratePhysicalDevices(&gpu_count,
			physical_devices);
		assert(err == vk::Result::eSuccess);
		/* For tri demo we just grab the first physical device */
		demo->gpu = physical_devices[0];
		free(physical_devices);
	}
	else {
		ERR_EXIT("vkEnumeratePhysicalDevices reported zero accessible devices."
			"\n\nDo you have a compatible Vulkan installable client"
			" driver (ICD) installed?\nPlease look at the Getting Started"
			" guide for additional information.\n",
			"vkEnumeratePhysicalDevices Failure");
	}

	/* Look for validation layers */
	if (demo->validate) {
		validation_found = 0;
		demo->enabled_layer_count = 0;
		uint32_t device_layer_count = 0;
		err =
			demo->gpu.enumerateDeviceLayerProperties(&device_layer_count, NULL);
		assert(err == vk::Result::eSuccess);

		if (device_layer_count > 0) {
			vk::LayerProperties *device_layers =
				(vk::LayerProperties *)malloc(sizeof(vk::LayerProperties) * device_layer_count);
			err = demo->gpu.enumerateDeviceLayerProperties(&device_layer_count,
				device_layers);
			assert(err == vk::Result::eSuccess);


			validation_found = demo_check_layers(device_validation_layer_count,
				demo->device_validation_layers,
				device_layer_count,
				device_layers);
			demo->enabled_layer_count = device_validation_layer_count;

			free(device_layers);
		}

		if (!validation_found) {
			ERR_EXIT("vkEnumerateDeviceLayerProperties failed to find "
				"a required validation layer.\n\n"
				"Please look at the Getting Started guide for additional "
				"information.\n",
				"vkCreateDevice Failure");
		}
	}

	/* Look for device extensions */
	uint32_t device_extension_count = 0;
	VkBool32 swapchainExtFound = 0;
	demo->enabled_extension_count = 0;
	memset(demo->extension_names, 0, sizeof(demo->extension_names));

	err = demo->gpu.enumerateDeviceExtensionProperties(NULL,
		&device_extension_count, NULL);
	assert(err == vk::Result::eSuccess);

	if (device_extension_count > 0) {
		vk::ExtensionProperties *device_extensions =
			(vk::ExtensionProperties *)malloc(sizeof(vk::ExtensionProperties) * device_extension_count);
		err = demo->gpu.enumerateDeviceExtensionProperties(
			NULL, &device_extension_count, device_extensions);
		assert(err == vk::Result::eSuccess);

		for (uint32_t i = 0; i < device_extension_count; i++) {
			if (!strcmp(VK_KHR_SWAPCHAIN_EXTENSION_NAME,
				device_extensions[i].extensionName)) {
				swapchainExtFound = 1;
				demo->extension_names[demo->enabled_extension_count++] =
					VK_KHR_SWAPCHAIN_EXTENSION_NAME;
			}
			assert(demo->enabled_extension_count < 64);
		}

		free(device_extensions);
	}

	if (!swapchainExtFound) {
		ERR_EXIT("vkEnumerateDeviceExtensionProperties failed to find "
			"the " VK_KHR_SWAPCHAIN_EXTENSION_NAME
			" extension.\n\nDo you have a compatible "
			"Vulkan installable client driver (ICD) installed?\nPlease "
			"look at the Getting Started guide for additional "
			"information.\n",
			"vkCreateInstance Failure");
	}

	if (demo->validate) {
		demo->CreateDebugReportCallback =
			(PFN_vkCreateDebugReportCallbackEXT)vkGetInstanceProcAddr(
				reinterpret_cast<VkInstance &>(demo->inst), "vkCreateDebugReportCallbackEXT");
		demo->DestroyDebugReportCallback =
			(PFN_vkDestroyDebugReportCallbackEXT)vkGetInstanceProcAddr(
				reinterpret_cast<VkInstance &>(demo->inst), "vkDestroyDebugReportCallbackEXT");
		if (!demo->CreateDebugReportCallback) {
			ERR_EXIT(
				"GetProcAddr: Unable to find vkCreateDebugReportCallbackEXT\n",
				"vkGetProcAddr Failure");
		}
		if (!demo->DestroyDebugReportCallback) {
			ERR_EXIT(
				"GetProcAddr: Unable to find vkDestroyDebugReportCallbackEXT\n",
				"vkGetProcAddr Failure");
		}
		demo->DebugReportMessage =
			(PFN_vkDebugReportMessageEXT)vkGetInstanceProcAddr(
				reinterpret_cast<VkInstance &>(demo->inst), "vkDebugReportMessageEXT");
		if (!demo->DebugReportMessage) {
			ERR_EXIT("GetProcAddr: Unable to find vkDebugReportMessageEXT\n",
				"vkGetProcAddr Failure");
		}

		VkDebugReportCallbackCreateInfoEXT dbgCreateInfo;
		dbgCreateInfo.sType = VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT;
		dbgCreateInfo.flags =
			VK_DEBUG_REPORT_ERROR_BIT_EXT | VK_DEBUG_REPORT_WARNING_BIT_EXT;
		dbgCreateInfo.pfnCallback = dbgFunc;
		dbgCreateInfo.pUserData = NULL;
		dbgCreateInfo.pNext = NULL;
		VkResult err_r U_ASSERT_ONLY = demo->CreateDebugReportCallback(
			reinterpret_cast<VkInstance &>(demo->inst), &dbgCreateInfo, NULL, &demo->msg_callback);

		switch (err) {
		case vk::Result::eSuccess:
			break;
		case vk::Result::eErrorOutOfDeviceMemory:
			ERR_EXIT("CreateDebugReportCallback: out of host memory\n",
				"CreateDebugReportCallback Failure");
			break;
		default:
			ERR_EXIT("CreateDebugReportCallback: unknown failure\n",
				"CreateDebugReportCallback Failure");
			break;
		}
	}

	// Having these GIPA queries of device extension entry points both
	// BEFORE and AFTER vkCreateDevice is a good test for the loader
	demo_get_entrypoints(demo);

	demo->gpu.getProperties(&demo->gpu_props);

	// Query with NULL data to get count
	demo->gpu.getQueueFamilyProperties(&demo->queue_count,
		NULL);

	demo->queue_props = (vk::QueueFamilyProperties *)malloc(
		demo->queue_count * sizeof(vk::QueueFamilyProperties));
	demo->gpu.getQueueFamilyProperties(&demo->queue_count, demo->queue_props);

	assert(demo->queue_count >= 1);

	// Graphics queue and MemMgr queue can be separate.
	// TODO: Add support for separate queues, including synchronization,
	//       and appropriate tracking for QueueSubmit
}

//Checked 6/7/16
static void demo_init_device(demoStruct* demo) {
	vk::Result U_ASSERT_ONLY err;

	float queue_priorities[1] = { 0.0 };
	const vk::DeviceQueueCreateInfo queue(

		vk::DeviceQueueCreateFlags(),
		demo->graphics_queue_node_index,
		1,
		queue_priorities);

	vk::DeviceCreateInfo device(

		vk::DeviceCreateFlags(),
		1,
		&queue,
		demo->enabled_layer_count,
		(const char *const *)
		((demo->validate)
			? demo->device_validation_layers
			: NULL),
		demo->enabled_extension_count,
		(const char *const *)demo->extension_names,
		NULL);

	err = demo->gpu.createDevice(&device, NULL, &demo->device);

	demo_get_entrypoints(demo);

	assert(err == vk::Result::eSuccess);
}

//Checked 6/7/16
static void demo_init_vk_swapchain(demoStruct* demo) {
	vk::Result U_ASSERT_ONLY err;
	VkResult U_ASSERT_ONLY err2; uint32_t i;

	// Create a WSI surface for the window:
#ifdef _WIN32
	vk::Win32SurfaceCreateInfoKHR createInfo(

		vk::Win32SurfaceCreateFlagsKHR(),
		demo->connection,
		demo->window
		);

	err =
		demo->inst.createWin32SurfaceKHR(&createInfo, NULL, &demo->surface);
	assert(err == vk::Result::eSuccess);
#else  // _WIN32
	VkXcbSurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = NULL;
	createInfo.flags = 0;
	createInfo.connection = demo->connection;
	createInfo.window = demo->window;

	err = vkCreateXcbSurfaceKHR(demo->inst, &createInfo, NULL, &demo->surface);
#endif // _WIN32

	// Iterate over each queue to learn whether it supports presenting:
	VkBool32 *supportsPresent =
		(VkBool32 *)malloc(demo->queue_count * sizeof(VkBool32));
	for (i = 0; i < demo->queue_count; i++) {
		demo->fpGetPhysicalDeviceSurfaceSupportKHR(
			reinterpret_cast<VkPhysicalDevice &>(demo->gpu), i,
			reinterpret_cast<VkSurfaceKHR &>(demo->surface), &supportsPresent[i]);
	}

	// Search for a graphics and a present queue in the array of queue
	// families, try to find one that supports both
	uint32_t graphicsQueueNodeIndex = UINT32_MAX;
	uint32_t presentQueueNodeIndex = UINT32_MAX;
	for (i = 0; i < demo->queue_count; i++) {
		if ((demo->queue_props[i].queueFlags & vk::QueueFlagBits::eGraphics) != vk::QueueFlags()) {
			if (graphicsQueueNodeIndex == UINT32_MAX) {
				graphicsQueueNodeIndex = i;
			}

			if (supportsPresent[i] == VK_TRUE) {
				graphicsQueueNodeIndex = i;
				presentQueueNodeIndex = i;
				break;
			}
		}
	}
	if (presentQueueNodeIndex == UINT32_MAX) {
		// If didn't find a queue that supports both graphics and present, then
		// find a separate present queue.
		for (uint32_t i = 0; i < demo->queue_count; ++i) {
			if (supportsPresent[i] == VK_TRUE) {
				presentQueueNodeIndex = i;
				break;
			}
		}
	}
	free(supportsPresent);

	// Generate error if could not find both a graphics and a present queue
	if (graphicsQueueNodeIndex == UINT32_MAX ||
		presentQueueNodeIndex == UINT32_MAX) {
		ERR_EXIT("Could not find a graphics and a present queue\n",
			"Swapchain Initialization Failure");
	}

	// TODO: Add support for separate queues, including presentation,
	//       synchronization, and appropriate tracking for QueueSubmit.
	// NOTE: While it is possible for an application to use a separate graphics
	//       and a present queues, this demo program assumes it is only using
	//       one:
	if (graphicsQueueNodeIndex != presentQueueNodeIndex) {
		ERR_EXIT("Could not find a common graphics and a present queue\n",
			"Swapchain Initialization Failure");
	}

	demo->graphics_queue_node_index = graphicsQueueNodeIndex;

	demo_init_device(demo);

	demo->device.getQueue(demo->graphics_queue_node_index, 0,
		&demo->queue);

	// Get the list of VkFormat's that are supported:
	uint32_t formatCount;
	err2 = demo->fpGetPhysicalDeviceSurfaceFormatsKHR(reinterpret_cast<VkPhysicalDevice &>(demo->gpu),
		reinterpret_cast<VkSurfaceKHR &>(demo->surface), &formatCount, NULL);
	assert(!err2);
	vk::SurfaceFormatKHR *surfFormats =
		(vk::SurfaceFormatKHR *)malloc(formatCount * sizeof(vk::SurfaceFormatKHR));
	err2 = demo->fpGetPhysicalDeviceSurfaceFormatsKHR(reinterpret_cast<VkPhysicalDevice &>(demo->gpu), reinterpret_cast<VkSurfaceKHR &>(demo->surface),
		&formatCount, reinterpret_cast<VkSurfaceFormatKHR *>(surfFormats));

	assert(!err2);
	// If the format list includes just one entry of VK_FORMAT_UNDEFINED,
	// the surface has no preferred format.  Otherwise, at least one
	// supported format will be returned.
	if (formatCount == 1 && surfFormats[0].format == vk::Format::eUndefined) {
		demo->format = vk::Format::eB8G8R8A8Unorm;
	}
	else {
		assert(formatCount >= 1);
		demo->format = (surfFormats[0].format);
	}
	demo->color_space = surfFormats[0].colorSpace;

	// Get Memory information and properties
	demo->gpu.getMemoryProperties(&demo->memory_properties);
	/*vkGetPhysicalDeviceMemoryProperties(
	reinterpret_cast<VkPhysicalDevice &>(demo->gpu),
	&reinterpret_cast<VkPhysicalDeviceMemoryProperties &>(demo->memory_properties));*/
}

static void demo_init_connection(demoStruct* demo) {
#ifndef _WIN32
	const xcb_setup_t *setup;
	xcb_screen_iterator_t iter;
	int scr;

	demo->connection = xcb_connect(NULL, &scr);
	if (demo->connection == NULL) {
		printf("Cannot find a compatible Vulkan installable client driver "
			"(ICD).\nExiting ...\n");
		fflush(stdout);
		exit(1);
	}

	setup = xcb_get_setup(demo->connection);
	iter = xcb_setup_roots_iterator(setup);
	while (scr-- > 0)
		xcb_screen_next(&iter);

	demo->screen = iter.data;
#endif // _WIN32
}

#ifdef _WIN32
static void demo_init(demoStruct* demo, HINSTANCE hInstance, LPSTR pCmdLine)
#else  // _WIN32
static void demo_init(demoStruct* demo, const int argc, const char *argv[])
#endif // _WIN32
{
	bool argv_error = false;

	memset(demo, 0, sizeof(*demo));

#ifdef _WIN32
	demo->connection = hInstance;
	strncpy(demo->name, APP_SHORT_NAME, APP_NAME_STR_LEN);

	if (strncmp(pCmdLine, "--use_staging", strlen("--use_staging")) == 0)
		demo->use_staging_buffer = true;
	else if (strncmp(pCmdLine, "--validate", strlen("--validate")) == 0)
		demo->use_staging_buffer = true;
	else if (strlen(pCmdLine) != 0) {
		fprintf(stdout, "Do not recognize argument \"%s\".\n", pCmdLine);
		argv_error = true;
	}
#else  // _WIN32
	for (int i = 0; i < argc; i++) {
		if (strncmp(argv[i], "--use_staging", strlen("--use_staging")) == 0)
			demo->use_staging_buffer = true;
		if (strncmp(argv[i], "--validate", strlen("--validate")) == 0)
			demo->validate = true;
	}
#endif // _WIN32
	if (argv_error) {
		fprintf(stdout, "Usage:\n  %s [--use_staging] [--validate]\n", APP_SHORT_NAME);
		fflush(stdout);
		exit(1);
	}

	demo_init_connection(demo);
	demo_init_vk(demo);

	demo->width = 300;
	demo->height = 300;
	demo->depthStencil = 1.0;
	demo->depthIncrement = -0.01f;
}

static void demo_cleanup(demoStruct* demo) {
	uint32_t i;

	demo->prepared = false;

	for (i = 0; i < demo->swapchainImageCount; i++) {
		demo->device.destroyFramebuffer(demo->framebuffers[i], NULL);
	}
	free(demo->framebuffers);
	demo->device.destroyDescriptorPool(demo->desc_pool, NULL);

	if (demo->setup_cmd) {
		demo->device.freeCommandBuffers(demo->cmd_pool, 1, &demo->setup_cmd);
	}
	demo->device.freeCommandBuffers(demo->cmd_pool, 1, &demo->draw_cmd);
	demo->device.destroyCommandPool(demo->cmd_pool, NULL);

	demo->device.destroyPipeline(demo->pipeline, NULL);
	demo->device.destroyRenderPass(demo->render_pass, NULL);
	demo->device.destroyPipelineLayout(demo->pipeline_layout, NULL);
	demo->device.destroyDescriptorSetLayout(demo->desc_layout, NULL);

	demo->device.destroyBuffer(demo->vertices.buf, NULL);
	demo->device.freeMemory(demo->vertices.mem, NULL);

	for (i = 0; i < DEMO_TEXTURE_COUNT; i++) {
		demo->device.destroyImageView(demo->textures[i].view, NULL);
		demo->device.destroyImage(demo->textures[i].image, NULL);
		demo->device.freeMemory(demo->textures[i].mem, NULL);
		demo->device.destroySampler(demo->textures[i].sampler, NULL);
	}

	for (i = 0; i < demo->swapchainImageCount; i++) {
		demo->device.destroyImageView(demo->buffers[i].view, NULL);
	}

	demo->device.destroyImageView(demo->depth.view, NULL);
	demo->device.destroyImage(demo->depth.image, NULL);
	demo->device.freeMemory(demo->depth.mem, NULL);

	demo->fpDestroySwapchainKHR(
		reinterpret_cast<VkDevice &>(demo->device),
		reinterpret_cast<VkSwapchainKHR &>(demo->swapchain), NULL);
	free(demo->buffers);

	demo->device.destroy(NULL);
	if (demo->validate) {
		demo->DestroyDebugReportCallback(
			reinterpret_cast<VkInstance &>(demo->inst), demo->msg_callback, NULL);

		//demo->inst.destroyDebugReportCallbackEXT(demo->msg_callback, NULL);
	}
	demo->inst.destroySurfaceKHR(demo->surface, NULL);
	demo->inst.destroy(NULL);

	free(demo->queue_props);

#ifndef _WIN32
	xcb_destroy_window(demo->connection, demo->window);
	xcb_disconnect(demo->connection);
	free(demo->atom_wm_delete_window);
#endif // _WIN32
}

static void demo_resize(demoStruct* demo) {
	uint32_t i;

	// Don't react to resize until after first initialization.
	if (!demo->prepared) {
		return;
	}
	// In order to properly resize the window, we must re-create the swapchain
	// AND redo the command buffers, etc.
	//
	// First, perform part of the demo_cleanup() function:
	demo->prepared = false;

	for (i = 0; i < demo->swapchainImageCount; i++) {
		demo->device.destroyFramebuffer(demo->framebuffers[i], NULL);
	}
	free(demo->framebuffers);
	demo->device.destroyDescriptorPool(demo->desc_pool, NULL);

	if (demo->setup_cmd) {
		demo->device.freeCommandBuffers(demo->cmd_pool, 1, &demo->setup_cmd);
	}
	demo->device.freeCommandBuffers(demo->cmd_pool, 1, &demo->draw_cmd);
	demo->device.destroyCommandPool(demo->cmd_pool, NULL);

	demo->device.destroyPipeline(demo->pipeline, NULL);
	demo->device.destroyRenderPass(demo->render_pass, NULL);
	demo->device.destroyPipelineLayout(demo->pipeline_layout, NULL);
	demo->device.destroyDescriptorSetLayout(demo->desc_layout, NULL);

	demo->device.destroyBuffer(demo->vertices.buf, NULL);
	demo->device.freeMemory(demo->vertices.mem, NULL);

	for (i = 0; i < DEMO_TEXTURE_COUNT; i++) {
		demo->device.destroyImageView(demo->textures[i].view, NULL);
		demo->device.destroyImage(demo->textures[i].image, NULL);
		demo->device.freeMemory(demo->textures[i].mem, NULL);
		demo->device.destroySampler(demo->textures[i].sampler, NULL);
	}

	for (i = 0; i < demo->swapchainImageCount; i++) {
		demo->device.destroyImageView(demo->buffers[i].view, NULL);
	}

	demo->device.destroyImageView(demo->depth.view, NULL);
	demo->device.destroyImage(demo->depth.image, NULL);
	demo->device.freeMemory(demo->depth.mem, NULL);

	free(demo->buffers);

	// Second, re-perform the demo_prepare() function, which will re-create the
	// swapchain:
	demo_prepare(demo);
}

#ifdef _WIN32
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
	LPSTR pCmdLine, int nCmdShow) {
	MSG msg;   // message
	bool done; // flag saying when app is complete

	AllocConsole();
	freopen("CONOUT$", "w", stdout);

	demo_init(&demo, hInstance, pCmdLine);
	demo_create_window(&demo);
	demo_init_vk_swapchain(&demo);

	demo_prepare(&demo);

	done = false; // initialize loop condition variable
				  /* main message loop*/
	while (!done) {
		PeekMessage(&msg, NULL, 0, 0, PM_REMOVE);
		if (msg.message == WM_QUIT) // check for a quit message
		{
			done = true; // if found, quit app
		}
		else {
			/* Translate and dispatch to event queue*/
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
		RedrawWindow(demo.window, NULL, NULL, RDW_INTERNALPAINT);
	}

	demo_cleanup(&demo);

	return (int)msg.wParam;
}
#else  // _WIN32
int main(const int argc, const char *argv[]) {
	struct demo demo;

	demo_init(&demo, argc, argv);
	demo_create_window(&demo);
	demo_init_vk_swapchain(&demo);

	demo_prepare(&demo);
	demo_run(&demo);

	demo_cleanup(&demo);

	return 0;
}
#endif // _WIN32
